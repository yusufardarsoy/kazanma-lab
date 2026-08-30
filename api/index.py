"""
Kazanma Lab - Vercel Serverless Gateway & Live Football Analytics API
"""

import json
import os
import sys
from http.server import BaseHTTPRequestHandler
import urllib.request
import urllib.parse

class handler(BaseHTTPRequestHandler):
    def do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        path = parsed.path
        
        self.send_response(200)
        self.send_header('Content-type', 'application/json; charset=utf-8')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        
        if path.startswith("/api/live"):
            # Fetch live scoreboard from ESPN
            try:
                url = "https://site.api.espn.com/apis/site/v2/sports/soccer/tur.1/scoreboard"
                req = urllib.request.Request(url, headers={'User-Agent': 'Kazanma-Lab/0.4'})
                with urllib.request.urlopen(req, timeout=10) as response:
                    data = json.loads(response.read().decode())
                
                events = data.get("events", [])
                live_matches = []
                for ev in events:
                    comp = ev.get("competitions", [{}])[0]
                    status = ev.get("status", {})
                    state = status.get("type", {}).get("state")
                    clock = status.get("displayClock", "")
                    detail = status.get("type", {}).get("detail", "")
                    
                    competitors = comp.get("competitors", [])
                    home = next((c for c in competitors if c.get("homeAway") == "home"), {})
                    away = next((c for c in competitors if c.get("homeAway") == "away"), {})
                    
                    live_matches.append({
                        "name": ev.get("name"),
                        "date": ev.get("date"),
                        "state": state,
                        "minute": clock,
                        "status": detail,
                        "home_team": home.get("team", {}).get("name"),
                        "home_score": home.get("score"),
                        "away_team": away.get("team", {}).get("name"),
                        "away_score": away.get("score")
                    })
                
                res = {
                    "status": "success",
                    "total_events": len(events),
                    "live_matches": live_matches
                }
            except Exception as e:
                res = {"status": "error", "message": str(e)}
            
            self.wfile.write(json.dumps(res, ensure_ascii=False, indent=2).encode('utf-8'))
            return
            
        # Default response
        res = {
            "name": "Kazanma Lab - Süper Lig Taktik & Tahmin Platformu",
            "version": "0.4.0",
            "status": "online",
            "engine": "NVIDIA Llama 3.2 Vision + 2D Gaussian Spatial Heatmap Radar",
            "endpoints": [
                "/api/live - Canlı Süper Lig Skorları ve Maç Durumları",
                "/heatmaps/ - Dinamik Oyuncu Isı Haritaları Galerisi"
            ]
        }
        self.wfile.write(json.dumps(res, ensure_ascii=False, indent=2).encode('utf-8'))
