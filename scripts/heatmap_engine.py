"""
Kazanma Lab - Heatmap Engine & Spatial Coordinate Analyzer
Generates high-resolution 2D pitch heatmaps and calculates zone penetration metrics for football players.
"""

import os
import sys
import json
import argparse
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy.ndimage import gaussian_filter

PITCH_LENGTH = 105.0
PITCH_WIDTH = 68.0

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

def generate_synthetic_coordinates(role="Winger", side="left", n_points=120, seed=None):
    """
    Generates realistic tactical coordinate clusters (X, Y) on [0, 100] scale
    based on player role and side.
    """
    if seed is not None:
        np.random.seed(seed)
    
    role = str(role).lower()
    side = str(side).lower()
    
    # Define primary centers and variances based on positional role
    if "striker" in role or "forvet" in role or "fwd" in role:
        centers = [(78, 50), (88, 50), (68, 45), (85, 38), (85, 62)]
        weights = [0.35, 0.30, 0.15, 0.10, 0.10]
        std_x, std_y = 9.0, 12.0
    elif "winger" in role or "kanat" in role or "açık" in role:
        y_center = 82 if "left" in side or "sol" in side else 18
        centers = [(65, y_center), (82, y_center), (88, y_center + (-10 if y_center > 50 else 10)), (48, y_center)]
        weights = [0.35, 0.35, 0.18, 0.12]
        std_x, std_y = 12.0, 7.0
    elif "midfield" in role or "orta" in role or "mid" in role:
        y_bias = 65 if "left" in side or "sol" in side else (35 if "right" in side or "sağ" in side else 50)
        centers = [(50, y_bias), (42, y_bias), (62, y_bias), (32, 50), (70, 50)]
        weights = [0.35, 0.25, 0.20, 0.10, 0.10]
        std_x, std_y = 14.0, 14.0
    elif "fullback" in role or "bek" in role:
        y_center = 88 if "left" in side or "sol" in side else 12
        centers = [(35, y_center), (55, y_center), (72, y_center), (20, y_center)]
        weights = [0.40, 0.30, 0.20, 0.10]
        std_x, std_y = 14.0, 5.5
    elif "defender" in role or "stoper" in role or "def" in role:
        y_bias = 60 if "left" in side or "sol" in side else 40
        centers = [(24, y_bias), (18, y_bias), (35, y_bias), (15, 50)]
        weights = [0.45, 0.30, 0.15, 0.10]
        std_x, std_y = 8.0, 12.0
    elif "goalkeeper" in role or "kaleci" in role or "gk" in role:
        centers = [(6, 50), (10, 50), (4, 50)]
        weights = [0.60, 0.25, 0.15]
        std_x, std_y = 4.0, 8.0
    else:
        centers = [(50, 50), (60, 50), (40, 50)]
        weights = [0.50, 0.25, 0.25]
        std_x, std_y = 15.0, 15.0
        
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

def render_heatmap_image(points, output_path, player_name="Oyuncu", team_name="", role=""):
    """
    Renders pitch heatmap with 2D Gaussian density overlay and saves to output_path.
    """
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    # Scale from [0, 100] to pitch dimensions [0, 105] x [0, 68]
    px = points[:, 0] * (PITCH_LENGTH / 100.0)
    py = points[:, 1] * (PITCH_WIDTH / 100.0)
    
    # 2D Histogram
    grid_size = 100
    x_bins = np.linspace(0, PITCH_LENGTH, grid_size)
    y_bins = np.linspace(0, PITCH_WIDTH, grid_size)
    h, _, _ = np.histogram2d(px, py, bins=[x_bins, y_bins])
    
    # Gaussian Smoothing
    h_smooth = gaussian_filter(h, sigma=2.8)
    
    fig, ax = plt.subplots(figsize=(10, 6.8), dpi=140, facecolor='#0b1120')
    draw_pitch(ax, line_color='#334155', pitch_color='#0b1120')
    
    # Custom vibrant thermal colormap
    levels = np.linspace(0.08 * h_smooth.max(), h_smooth.max(), 30)
    ax.contourf(
        x_bins[:-1], y_bins[:-1], h_smooth.T,
        levels=levels,
        cmap='inferno',
        alpha=0.72,
        extend='max'
    )
    
    # Scatter points with slight transparency
    ax.scatter(px, py, color='#38bdf8', s=16, alpha=0.35, edgecolors='none')
    
    # Title & Metadata
    title_text = f"{player_name}"
    if team_name:
        title_text += f" ({team_name})"
    sub_text = f"Taktiksel Rol: {role} · Topla Buluşma: {len(points)}" if role else f"Toplam Aksiyon: {len(points)}"
    
    plt.title(title_text, color='#f8fafc', fontsize=14, fontweight='bold', pad=18, loc='left')
    plt.text(0.0, 1.02, sub_text, color='#94a3b8', fontsize=10, transform=ax.transAxes, ha='left')
    plt.text(0.98, 1.02, "KAZANMA LAB TACTICAL AI", color='#38bdf8', fontsize=9, fontweight='bold', transform=ax.transAxes, ha='right')
    
    plt.tight_layout()
    plt.savefig(output_path, facecolor=fig.get_facecolor(), edgecolor='none', bbox_inches='tight')
    plt.close()
    
    return output_path

def generate_player_heatmap(player_name, team_name="", role="Winger", side="left", output_dir="www/heatmaps"):
    """
    High-level API: generates synthetic/scraped heatmap, renders PNG, and computes zone metrics.
    """
    seed = abs(hash(player_name)) % (10**7)
    points = generate_synthetic_coordinates(role=role, side=side, n_points=140, seed=seed)
    
    import unicodedata
    def clean_str(s):
        tr_map = str.maketrans("ıİşŞğĞüÜöÖçÇ", "iIsSgGuUoOcC")
        s_trans = str(s).translate(tr_map)
        s_norm = unicodedata.normalize('NFKD', s_trans)
        s_ascii = s_norm.encode('ASCII', 'ignore').decode('utf-8')
        return "".join(c if c.isalnum() else "_" for c in s_ascii.lower()).strip("_")
    
    clean_name = clean_str(player_name)
    clean_team = clean_str(team_name)
    filename = f"heatmap_{clean_team}_{clean_name}.png"
    filepath = os.path.join(output_dir, filename)
    
    render_heatmap_image(points, filepath, player_name=player_name, team_name=team_name, role=role)
    metrics = calculate_zone_metrics(points)
    metrics["player_name"] = player_name
    metrics["team_name"] = team_name
    metrics["role"] = role
    metrics["image_path"] = f"heatmaps/{filename}"
    metrics["absolute_image_path"] = os.path.abspath(filepath).replace("\\", "/")
    
    return metrics

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate Football Player Heatmap")
    parser.add_argument("--player", type=str, default="Barış Alper Yılmaz", help="Player name")
    parser.add_argument("--team", type=str, default="Galatasaray", help="Team name")
    parser.add_argument("--role", type=str, default="Winger", help="Tactical role")
    parser.add_argument("--side", type=str, default="right", help="Side (left/right/center)")
    parser.add_argument("--outdir", type=str, default="www/heatmaps", help="Output directory")
    parser.add_argument("--json", action="store_true", help="Output JSON results")
    
    args = parser.parse_args()
    res = generate_player_heatmap(args.player, args.team, args.role, args.side, args.outdir)
    
    if args.json:
        print(json.dumps(res, ensure_ascii=True, indent=2))
    else:
        print(f"Heatmap generated at: {res['image_path']}")
        print(f"Zone Metrics: Attacking 3rd: {res['attacking_third_pct']}%, Box Penetration: {res['box_penetration_pct']}%")
