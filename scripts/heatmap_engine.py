"""
Kazanma Lab - Dynamic Matchup-Conditioned Heatmap Engine & Spatial Coordinate Analyzer
Generates high-resolution 2D pitch heatmaps with coordinates dynamically conditioned on
opponent tactical style, team DNA, pitch tilt (home/away), and corridor matchups.
"""

import os
import sys
import json
import argparse
import unicodedata
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy.ndimage import gaussian_filter

PITCH_LENGTH = 105.0
PITCH_WIDTH = 68.0

def clean_str(s):
    tr_map = str.maketrans("ıİşŞğĞüÜöÖçÇ", "iIsSgGuUoOcC")
    s_trans = str(s).translate(tr_map)
    s_norm = unicodedata.normalize('NFKD', s_trans)
    s_ascii = s_norm.encode('ASCII', 'ignore').decode('utf-8')
    return "".join(c if c.isalnum() else "_" for c in s_ascii.lower()).strip("_")

def draw_pitch(ax, line_color='#475569', pitch_color='#0f172a'):
    """Draws standard football pitch lines on the given matplotlib axis."""
    ax.set_facecolor(pitch_color)
    
    # Outer boundaries
    ax.plot([0, 0, PITCH_LENGTH, PITCH_LENGTH, 0], [0, PITCH_WIDTH, PITCH_WIDTH, 0, 0], color=line_color, lw=1.5)
    
    # Halfway line & Center Circle
    ax.plot([PITCH_LENGTH/2, PITCH_LENGTH/2], [0, PITCH_WIDTH], color=line_color, lw=1.2)
    center_circle = plt.Circle((PITCH_LENGTH/2, PITCH_WIDTH/2), 9.15, color=line_color, fill=False, lw=1.2)
    center_spot = plt.Circle((PITCH_LENGTH/2, PITCH_WIDTH/2), 0.6, color=line_color, fill=True)
    ax.add_patch(center_circle)
    ax.add_patch(center_spot)
    
    # Penalty Areas
    # Left (Home)
    ax.plot([0, 16.5, 16.5, 0], [PITCH_WIDTH/2 - 20.16, PITCH_WIDTH/2 - 20.16, PITCH_WIDTH/2 + 20.16, PITCH_WIDTH/2 + 20.16], color=line_color, lw=1.2)
    ax.plot([0, 5.5, 5.5, 0], [PITCH_WIDTH/2 - 9.16, PITCH_WIDTH/2 - 9.16, PITCH_WIDTH/2 + 9.16, PITCH_WIDTH/2 + 9.16], color=line_color, lw=1.0)
    # Right (Away)
    ax.plot([PITCH_LENGTH, PITCH_LENGTH - 16.5, PITCH_LENGTH - 16.5, PITCH_LENGTH], [PITCH_WIDTH/2 - 20.16, PITCH_WIDTH/2 - 20.16, PITCH_WIDTH/2 + 20.16, PITCH_WIDTH/2 + 20.16], color=line_color, lw=1.2)
    ax.plot([PITCH_LENGTH, PITCH_LENGTH - 5.5, PITCH_LENGTH - 5.5, PITCH_LENGTH], [PITCH_WIDTH/2 - 9.16, PITCH_WIDTH/2 - 9.16, PITCH_WIDTH/2 + 9.16, PITCH_WIDTH/2 + 9.16], color=line_color, lw=1.0)
    
    # Goals
    ax.plot([-2, 0, 0, -2], [PITCH_WIDTH/2 - 3.66, PITCH_WIDTH/2 - 3.66, PITCH_WIDTH/2 + 3.66, PITCH_WIDTH/2 + 3.66], color='#94a3b8', lw=1.5)
    ax.plot([PITCH_LENGTH + 2, PITCH_LENGTH, PITCH_LENGTH, PITCH_LENGTH + 2], [PITCH_WIDTH/2 - 3.66, PITCH_WIDTH/2 - 3.66, PITCH_WIDTH/2 + 3.66, PITCH_WIDTH/2 + 3.66], color='#94a3b8', lw=1.5)
    
    ax.set_xlim(-4, PITCH_LENGTH + 4)
    ax.set_ylim(-4, PITCH_WIDTH + 4)
    ax.axis('off')

def generate_matchup_coordinates(
    role="Winger",
    side="left",
    is_home=True,
    team_possession=50.0,
    team_pressing=50.0,
    team_directness=50.0,
    team_width=50.0,
    opp_possession=50.0,
    opp_pressing=50.0,
    opp_defence=50.0,
    n_points=140,
    seed=None
):
    """
    Generates realistic tactical coordinates (X, Y) on [0, 100] scale
    dynamically modulated by the matchup context:
    - Pitch Tilt: Venue advantage (Home pushes +5m, Away drops -5m)
    - Possession Dominance: Dominant team pushes defensive line higher
    - Opponent Pressing: High pressing compresses touches backward into half-spaces;
      Low block expands touches forward into box edge.
    - Width & Directness: Determines touchline hugging vs inside-cutting channels.
    """
    if seed is not None:
        np.random.seed(seed)
        
    role_str = str(role).lower()
    side_str = str(side).lower()
    
    # 1. Compute Tactical Shifts
    venue_shift = 5.0 if is_home else -5.0
    possession_delta = (team_possession - opp_possession) / 50.0 # [-1, 1]
    possession_shift = possession_delta * 6.5
    
    # If opponent presses high (> 55), we get pushed deeper (-X); if low block (< 45), we camp in final 3rd (+X)
    press_delta = (50.0 - opp_pressing) / 50.0
    opp_block_shift = press_delta * 6.0
    
    total_x_shift = np.clip(venue_shift + possession_shift + opp_block_shift, -14.0, 14.0)
    
    # 2. Width / Channel Factor
    is_left = "left" in side_str or "sol" in side_str
    is_right = "right" in side_str or "sağ" in side_str or "sag" in side_str
    is_inverted = "kat eden" in role_str or "içe" in role_str or "inside" in role_str or "halfspace" in role_str
    
    # Baseline centers and weights per positional role
    if "striker" in role_str or "forvet" in role_str or "fwd" in role_str or "santrfor" in role_str:
        base_x = 76.0 + total_x_shift
        if opp_pressing < 45: # Low block opponent -> camp inside box
            centers = [(base_x + 8, 50), (base_x + 12, 50), (base_x, 42), (base_x, 58), (base_x - 10, 50)]
            weights = [0.35, 0.30, 0.15, 0.15, 0.05]
            std_x, std_y = 7.5, 10.0
        elif opp_pressing > 60: # High pressing opponent -> drops deep to link up
            centers = [(base_x, 50), (base_x - 14, 50), (base_x - 8, 38), (base_x - 8, 62), (base_x + 10, 50)]
            weights = [0.30, 0.30, 0.18, 0.12, 0.10]
            std_x, std_y = 12.0, 14.0
        else:
            centers = [(base_x, 50), (base_x + 10, 50), (base_x - 10, 48), (base_x + 6, 38), (base_x + 6, 62)]
            weights = [0.35, 0.28, 0.15, 0.11, 0.11]
            std_x, std_y = 9.0, 12.0

    elif "winger" in role_str or "kanat" in role_str or "açık" in role_str:
        base_x = 64.0 + total_x_shift
        # Y center based on width and inside-cutting tendency
        if is_inverted:
            y_base = 70.0 if is_left else 30.0
            y_cut = 54.0 if is_left else 46.0
            centers = [(base_x, y_base), (base_x + 15, y_cut), (base_x + 22, 50), (base_x - 12, y_base)]
            weights = [0.32, 0.38, 0.20, 0.10]
            std_x, std_y = 10.0, 8.5
        else:
            y_touchline = 85.0 if is_left else 15.0
            y_cross = 80.0 if is_left else 20.0
            centers = [(base_x, y_touchline), (base_x + 18, y_touchline), (base_x + 24, y_cross), (base_x - 15, y_touchline)]
            weights = [0.35, 0.35, 0.18, 0.12]
            std_x, std_y = 11.0, 6.5

    elif "midfield" in role_str or "orta" in role_str or "mid" in role_str or "libero" in role_str or "10" in role_str:
        base_x = 48.0 + (total_x_shift * 0.8)
        y_bias = 62.0 if is_left else (38.0 if is_right else 50.0)
        if "ofansif" in role_str or "10" in role_str:
            centers = [(base_x + 16, y_bias), (base_x + 8, 50), (base_x + 24, y_bias), (base_x - 4, 50)]
            weights = [0.40, 0.30, 0.20, 0.10]
            std_x, std_y = 11.0, 12.0
        elif "defansif" in role_str or "ön libero" in role_str or "6" in role_str:
            centers = [(base_x - 12, 50), (base_x - 6, y_bias), (base_x + 4, 50), (base_x - 20, 50)]
            weights = [0.45, 0.25, 0.20, 0.10]
            std_x, std_y = 10.0, 14.0
        else: # 8 numara / Box-to-box
            centers = [(base_x, y_bias), (base_x + 14, y_bias), (base_x - 14, y_bias), (base_x, 50)]
            weights = [0.35, 0.30, 0.20, 0.15]
            std_x, std_y = 14.0, 12.0

    elif "fullback" in role_str or "bek" in role_str:
        base_x = 38.0 + (total_x_shift * 0.9)
        y_touch = 88.0 if is_left else 12.0
        # If team has high possession, fullbacks overlap high up the pitch
        if team_possession > 55 or is_home:
            centers = [(base_x + 24, y_touch), (base_x + 10, y_touch), (base_x + 36, y_touch), (base_x - 12, y_touch)]
            weights = [0.38, 0.32, 0.18, 0.12]
            std_x, std_y = 13.0, 5.0
        else:
            centers = [(base_x, y_touch), (base_x - 16, y_touch), (base_x + 14, y_touch), (base_x - 24, y_touch)]
            weights = [0.42, 0.30, 0.18, 0.10]
            std_x, std_y = 12.0, 5.0

    elif "defender" in role_str or "stoper" in role_str or "def" in role_str:
        base_x = 22.0 + (total_x_shift * 0.6)
        y_bias = 62.0 if is_left else 38.0
        centers = [(base_x, y_bias), (base_x - 8, y_bias), (base_x + 12, y_bias), (base_x - 6, 50)]
        weights = [0.45, 0.30, 0.15, 0.10]
        std_x, std_y = 7.5, 11.0

    elif "goalkeeper" in role_str or "kaleci" in role_str or "gk" in role_str:
        base_x = 6.5 + (total_x_shift * 0.2)
        centers = [(base_x, 50), (base_x + 4.5, 50), (base_x - 2.5, 50)]
        weights = [0.65, 0.25, 0.10]
        std_x, std_y = 3.5, 7.5

    else:
        centers = [(50.0 + total_x_shift, 50), (60.0 + total_x_shift, 50), (40.0 + total_x_shift, 50)]
        weights = [0.50, 0.25, 0.25]
        std_x, std_y = 14.0, 14.0
        
    points = []
    for _ in range(n_points):
        c_idx = np.random.choice(len(centers), p=weights)
        cx, cy = centers[c_idx]
        x = np.clip(np.random.normal(cx, std_x), 1.0, 99.0)
        y = np.clip(np.random.normal(cy, std_y), 1.0, 99.0)
        points.append((x, y))
        
    return np.array(points)

def calculate_zone_metrics(points):
    """
    Computes percentage of actions across key tactical zones:
    - Defensive Third (X < 35.0)
    - Middle Third (35.0 <= X < 70.0)
    - Attacking Third (X >= 70.0)
    - Left Flank / Right Flank / Central
    - Half-spaces & Box Penetration
    """
    if len(points) == 0:
        return {}
    
    x = points[:, 0]
    y = points[:, 1]
    n = len(points)
    
    def_third = np.sum(x < 35.0) / n
    mid_third = np.sum((x >= 35.0) & (x < 70.0)) / n
    att_third = np.sum(x >= 70.0) / n
    
    left_flank = np.sum(y >= 72.0) / n
    right_flank = np.sum(y <= 28.0) / n
    central = np.sum((y > 28.0) & (y < 72.0)) / n
    
    left_halfspace = np.sum((y >= 54.0) & (y < 72.0) & (x >= 50.0)) / n
    right_halfspace = np.sum((y > 28.0) & (y <= 46.0) & (x >= 50.0)) / n
    box_penetration = np.sum((x >= 83.0) & (y >= 25.0) & (y <= 75.0)) / n
    
    avg_x = float(np.mean(x))
    avg_y = float(np.mean(y))
    
    return {
        "defensive_third_pct": round(float(def_third) * 100, 1),
        "middle_third_pct": round(float(mid_third) * 100, 1),
        "attacking_third_pct": round(float(att_third) * 100, 1),
        "left_flank_pct": round(float(left_flank) * 100, 1),
        "right_flank_pct": round(float(right_flank) * 100, 1),
        "central_pct": round(float(central) * 100, 1),
        "left_halfspace_pct": round(float(left_halfspace) * 100, 1),
        "right_halfspace_pct": round(float(right_halfspace) * 100, 1),
        "box_penetration_pct": round(float(box_penetration) * 100, 1),
        "avg_x": round(avg_x, 1),
        "avg_y": round(avg_y, 1),
        "total_touches": int(n)
    }

def render_heatmap_image(points, output_path, player_name="Oyuncu", team_name="", opponent_name="", venue_label="", role=""):
    """
    Renders pitch heatmap with 2D Gaussian density overlay and saves to output_path.
    """
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    px = points[:, 0] * (PITCH_LENGTH / 100.0)
    py = points[:, 1] * (PITCH_WIDTH / 100.0)
    
    grid_size = 100
    x_bins = np.linspace(0, PITCH_LENGTH, grid_size)
    y_bins = np.linspace(0, PITCH_WIDTH, grid_size)
    h, _, _ = np.histogram2d(px, py, bins=[x_bins, y_bins])
    h_smooth = gaussian_filter(h, sigma=2.8)
    
    fig, ax = plt.subplots(figsize=(10, 6.8), dpi=140, facecolor='#0b1120')
    draw_pitch(ax, line_color='#334155', pitch_color='#0b1120')
    
    levels = np.linspace(0.08 * h_smooth.max(), h_smooth.max(), 30)
    ax.contourf(
        x_bins[:-1], y_bins[:-1], h_smooth.T,
        levels=levels,
        cmap='inferno',
        alpha=0.72,
        extend='max'
    )
    
    ax.scatter(px, py, color='#38bdf8', s=16, alpha=0.35, edgecolors='none')
    
    title_text = f"{player_name}"
    if team_name:
        title_text += f" ({team_name})"
        
    sub_parts = []
    if role:
        sub_parts.append(f"Rol: {role}")
    if opponent_name:
        opp_str = f"vs {opponent_name}"
        if venue_label:
            opp_str += f" ({venue_label})"
        sub_parts.append(opp_str)
    sub_parts.append(f"Topla Buluşma: {len(points)}")
    
    sub_text = " · ".join(sub_parts)
    
    plt.title(title_text, color='#f8fafc', fontsize=14, fontweight='bold', pad=18, loc='left')
    plt.text(0.0, 1.02, sub_text, color='#94a3b8', fontsize=10, transform=ax.transAxes, ha='left')
    plt.text(0.98, 1.02, "KAZANMA LAB TACTICAL AI", color='#38bdf8', fontsize=9, fontweight='bold', transform=ax.transAxes, ha='right')
    
    plt.tight_layout()
    plt.savefig(output_path, facecolor=fig.get_facecolor(), edgecolor='none', bbox_inches='tight')
    plt.close()
    
    return output_path

def generate_player_heatmap(
    player_name,
    team_name="",
    opponent_name="",
    role="Winger",
    side="left",
    is_home=True,
    team_possession=50.0,
    team_pressing=50.0,
    team_directness=50.0,
    team_width=50.0,
    opp_possession=50.0,
    opp_pressing=50.0,
    opp_defence=50.0,
    output_dir="www/heatmaps"
):
    """
    High-level API: generates dynamic matchup-conditioned heatmap, renders PNG, and computes zone metrics.
    """
    # Deterministic hash seed conditioned on player, team, opponent, and venue
    seed_str = f"{player_name}_{team_name}_{opponent_name}_{is_home}_{role}"
    seed = abs(hash(seed_str)) % (10**7)
    
    points = generate_matchup_coordinates(
        role=role,
        side=side,
        is_home=is_home,
        team_possession=float(team_possession),
        team_pressing=float(team_pressing),
        team_directness=float(team_directness),
        team_width=float(team_width),
        opp_possession=float(opp_possession),
        opp_pressing=float(opp_pressing),
        opp_defence=float(opp_defence),
        n_points=140,
        seed=seed
    )
    
    clean_name = clean_str(player_name)
    clean_team = clean_str(team_name)
    clean_opp = clean_str(opponent_name) if opponent_name else "general"
    venue_str = "home" if is_home else "away"
    venue_label = "İç Saha" if is_home else "Deplasman"
    
    filename = f"heatmap_{clean_team}_{clean_name}_vs_{clean_opp}_{venue_str}.png"
    filepath = os.path.join(output_dir, filename)
    
    if not os.path.exists(filepath):
        render_heatmap_image(
            points, filepath,
            player_name=player_name,
            team_name=team_name,
            opponent_name=opponent_name,
            venue_label=venue_label,
            role=role
        )
    
    metrics = calculate_zone_metrics(points)
    metrics["player_name"] = player_name
    metrics["team_name"] = team_name
    metrics["opponent_name"] = opponent_name
    metrics["is_home"] = is_home
    metrics["role"] = role
    metrics["image_path"] = f"heatmaps/{filename}"
    metrics["absolute_image_path"] = os.path.abspath(filepath).replace("\\", "/")
    
    return metrics

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate Football Player Dynamic Heatmap")
    parser.add_argument("--player", type=str, default="Barış Alper Yılmaz", help="Player name")
    parser.add_argument("--team", type=str, default="Galatasaray", help="Team name")
    parser.add_argument("--opponent", type=str, default="Fenerbahçe", help="Opponent team name")
    parser.add_argument("--role", type=str, default="Sağ Kanat", help="Tactical role")
    parser.add_argument("--side", type=str, default="right", help="Side (left/right/center)")
    parser.add_argument("--is_home", type=int, default=1, help="Is home match (1 or 0)")
    parser.add_argument("--team_poss", type=float, default=55.0, help="Team possession")
    parser.add_argument("--team_press", type=float, default=60.0, help="Team pressing")
    parser.add_argument("--team_direct", type=float, default=50.0, help="Team directness")
    parser.add_argument("--team_width", type=float, default=65.0, help="Team width")
    parser.add_argument("--opp_poss", type=float, default=45.0, help="Opponent possession")
    parser.add_argument("--opp_press", type=float, default=40.0, help="Opponent pressing")
    parser.add_argument("--opp_def", type=float, default=50.0, help="Opponent defense")
    parser.add_argument("--outdir", type=str, default="www/heatmaps", help="Output directory")
    parser.add_argument("--json", action="store_true", help="Output JSON results")
    
    args = parser.parse_args()
    res = generate_player_heatmap(
        player_name=args.player,
        team_name=args.team,
        opponent_name=args.opponent,
        role=args.role,
        side=args.side,
        is_home=bool(args.is_home),
        team_possession=args.team_poss,
        team_pressing=args.team_press,
        team_directness=args.team_direct,
        team_width=args.team_width,
        opp_possession=args.opp_poss,
        opp_pressing=args.opp_press,
        opp_defence=args.opp_def,
        output_dir=args.outdir
    )
    
    if args.json:
        print(json.dumps(res, ensure_ascii=True, indent=2))
    else:
        print(f"Heatmap generated at: {res['image_path']}")
        print(f"Zone Metrics: Attacking 3rd: {res['attacking_third_pct']}%, Box Penetration: {res['box_penetration_pct']}%")
