"""
Kazanma Lab - NVIDIA NIM Tactical Intelligence Agent
Uses Llama 3.2 on NVIDIA NIM to generate deep tactical scout reports based on player & team spatial heatmaps.
"""

import os
import sys
import json
import argparse
import requests
import time

DEFAULT_NVIDIA_URL = "https://integrate.api.nvidia.com/v1/chat/completions"
DEFAULT_API_KEY = "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"
DEFAULT_MODEL = "meta/llama-3.2-11b-vision-instruct"

def load_env_key():
    key = os.environ.get("NVIDIA_API_KEY", "")
    if key:
        return key
    env_path = ".env"
    if os.path.exists(env_path):
        with open(env_path, "r", encoding="utf-8") as f:
            for line in f:
                if line.startswith("NVIDIA_API_KEY="):
                    return line.strip().split("=", 1)[1].strip().strip('"').strip("'")
    return DEFAULT_API_KEY

def generate_tactical_report(
    match_name,
    home_team,
    away_team,
    home_player="Barış Alper Yılmaz",
    home_player_role="Sağ Kanat / Hücum",
    home_metrics=None,
    away_player="Rakip Sol Bek",
    away_player_role="Sol Bek / Savunma",
    away_metrics=None,
    model=DEFAULT_MODEL,
    api_key=None,
    timeout=30
):
    if not api_key:
        api_key = load_env_key()
    
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
        "Accept": "application/json"
    }
    
    home_metrics = home_metrics or {
        "attacking_third_pct": 65.0,
        "right_flank_pct": 82.0,
        "box_penetration_pct": 15.0
    }
    away_metrics = away_metrics or {
        "defensive_third_pct": 48.0,
        "left_flank_pct": 78.0,
        "box_penetration_pct": 2.0
    }
    
    system_prompt = (
        "Sen dünyanın en iyi futbol taktik analisti ve UEFA Pro Lisanslı scout uzmanısın. "
        "Oyuncuların saha içi ısı haritalarını (heatmap), topla buluşma yoğunluklarını ve taktiksel rollerini "
        "derinlemesine inceleyerek Türkçe, net, somut ve teknik bir maç önü taktik raporu hazırlarsın."
    )
    
    user_prompt = f"""
Maç: {match_name} ({home_team} vs {away_team})

[OYUNCU 1 · EV SAHİBİ]
- İsim & Rol: {home_player} ({home_player_role} - {home_team})
- Isı Haritası Yoğunluğu: 3. Bölge (Hücum): %{home_metrics.get('attacking_third_pct', 60)}, Sağ/Sol Kanat: %{home_metrics.get('right_flank_pct', home_metrics.get('left_flank_pct', 70))}, Ceza Sahası Girişi: %{home_metrics.get('box_penetration_pct', 15)}

[OYUNCU 2 · DEPLASMAN RAKİP KORİDOR]
- İsim & Rol: {away_player} ({away_player_role} - {away_team})
- Isı Haritası Yoğunluğu: 1. Bölge (Savunma): %{away_metrics.get('defensive_third_pct', 50)}, Kanat Savunması: %{away_metrics.get('left_flank_pct', away_metrics.get('right_flank_pct', 75))}, Hücuma Çıkış: %{away_metrics.get('attacking_third_pct', 20)}

Lütfen bu iki oyuncunun sahadaki doğrudan koridor eşleşmesini ve takımların oyun stillerini analiz eden 3 maddelik net bir scout raporu hazırla:
1. ⚔️ **Koridor Çakışması ve Isı Haritası Üstünlüğü**: Hangi oyuncu kendi bölgesinde alan yaratır veya açık verir?
2. 🎯 **Taktiksel Kilit Zafiyet / Fırsat**: Bu eşleşmeden doğacak en kritik pozisyon veya gol tehlikesi nedir?
3. 🔮 **Model & Bahis/Skor Yansıması**: Bu bireysel dinamik maçın toplam golüne, ilk yarı/maç sonu (0/1 vb.) kilitlenmesine veya kart olasılıklarına nasıl etki eder?

Yanıtını profesyonel futbol terminolojisiyle ve akıcı Türkçe ile yaz.
"""
    
    payload = {
        "model": model,
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ],
        "max_tokens": 600,
        "temperature": 0.6
    }
    
    t0 = time.time()
    try:
        resp = requests.post(DEFAULT_NVIDIA_URL, headers=headers, json=payload, timeout=timeout)
        duration = round(time.time() - t0, 2)
        if resp.status_code == 200:
            content = resp.json()["choices"][0]["message"]["content"]
            return {
                "status": "success",
                "model": model,
                "duration_seconds": duration,
                "report_text": content,
                "home_player": home_player,
                "away_player": away_player,
                "match_name": match_name
            }
        else:
            return {
                "status": "error",
                "status_code": resp.status_code,
                "error_message": resp.text,
                "duration_seconds": duration
            }
    except Exception as e:
        return {
            "status": "error",
            "error_message": str(e),
            "duration_seconds": round(time.time() - t0, 2)
        }

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="NVIDIA NIM Football Tactical Scout Agent")
    parser.add_argument("--match", type=str, default="Galatasaray vs Çorum FK", help="Match name")
    parser.add_argument("--home", type=str, default="Galatasaray", help="Home team")
    parser.add_argument("--away", type=str, default="Çorum FK", help="Away team")
    parser.add_argument("--home_player", type=str, default="Barış Alper Yılmaz", help="Home player")
    parser.add_argument("--home_role", type=str, default="Sağ Kanat", help="Home role")
    parser.add_argument("--away_player", type=str, default="Erkan Kaş", help="Away player")
    parser.add_argument("--away_role", type=str, default="Sol Bek", help="Away role")
    parser.add_argument("--json", action="store_true", help="Output JSON format")
    
    args = parser.parse_args()
    report = generate_tactical_report(
        match_name=args.match,
        home_team=args.home,
        away_team=args.away,
        home_player=args.home_player,
        home_player_role=args.home_role,
        away_player=args.away_player,
        away_player_role=args.away_role
    )
    
    if args.json:
        print(json.dumps(report, ensure_ascii=True, indent=2))
    else:
        if report.get("status") == "success":
            print(f"=== NVIDIA NIM ({report['model']}) Taktik Raporu ===")
            print(report["report_text"])
        else:
            print(f"Hata: {report.get('error_message')}")
