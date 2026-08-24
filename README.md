# Kazanma Lab

Kazanma Lab, “Moneyball / Kazanma Sanatı” yaklaşımını futbol maçlarına uyarlayan kişisel bir R Shiny analiz odasıdır. Muhtemel ilk 11'i, maç sonucu ve skor olasılıklarını, golcü/kart adaylarını ve iki takımın oyun stili eşleşmesini tek ekranda gösterir.

> Mevcut sürüm yalnızca **2026-27 Trendyol Süper Lig** maçlarını açar. TFF fikstürü, 2025-26 resmi lig performansı, kadro değeri ve kamuya açık takım incelemelerinden oluşturulmuş takım öncülleri kullanılır. Olasılıklar kesin sonuç veya bahis tavsiyesi değildir.

## Şu anda çalışanlar

- Şifreli, oturum bazlı kişisel giriş
- 10 hatalı denemeden sonra 20 dakikalık oturum kilidi
- Poisson tabanlı ev / beraberlik / deplasman ve kesin skor matrisi
- Pozisyon kotasına göre muhtemel ilk 11 (1 kaleci, 4 savunma, 3 orta saha, 3 hücum)
- İlk 11 olasılığı ve beklenen dakika ağırlıklı gol/kart projeksiyonu
- Pres, topa sahip olma, dikeylik, genişlik, geçiş, duran top ve disiplin karşılaştırması
- Kural tabanlı taktik eşleşme notları
- Süper Lig'deki 18 takım için teknik direktör, oyun kimliği, güçlü yön, zayıflık ve profil güveni
- TFF'nin 34 haftalık resmi eşleşme kataloğunda 306 aranabilir maç; açıklanan gün/saatler ayrıca işaretli
- Takım filtresi ve takım/maç adına göre fikstür araması
- Kocaelispor–Amed için doğrulanmış eksikler, güncel muhtemel 11 ve maça özel xG düzeltmesi
- Kullanıcının gönderdiği İddaa görüntüsünde 1X2, gol, KG, ilk yarı, ilk gol, doğru skor ve golcü oranlarını modelle karşılaştıran oran radarı
- Marjsız piyasa olasılığı, başabaş olasılığı, model farkı, beklenen değer, veri bayatlığı ve risk etiketi
- Her analiz anını SQLite'a kaydeden model hafızası
- Ekrandan gerçek skor girişi veya toplu CSV içe aktarma
- İleriye dönük 1X2 isabeti, Brier skoru ve log loss; sonuçtan sonra üretilen tahminleri ölçümden çıkaran zaman kontrolü
- API-Football için fikstür, tahmin, resmi ilk 11 ve sakatlık/ceza snapshot bağlantısı
- Docker ve GitHub Actions hazırlığı

## Ekranlar

1. **Maç merkezi:** kazanma olasılıkları, beklenen gol, en olası skor ve skor matrisi.
2. **Oran radarı:** oran, başabaş/marjsız piyasa olasılığı, model olasılığı, fark ve risk.
3. **Muhtemel 11:** oyuncu bazında başlama olasılığı ve rol.
4. **Stil savaşı:** iki takımın normalize oyun profili ve taktik kırılma noktaları.
5. **Süper Lig DNA:** 18 takımın teknik direktörü, taktik özeti, güçlü/zayıf yönleri ve veri güveni.
6. **Oyuncu radarları:** güncel isimli kadro yoksa açıkça işaretlenmiş rol bazlı gol/kart öncülleri.
7. **Model hafızası:** analiz geçmişi, maç-sonu sonuç girişi ve ileriye dönük kalibrasyon ölçümleri.

## Yerelde çalıştırma

R 4.4+ kurulu olmalı.

```r
install.packages(c(
  "bslib", "DBI", "dplyr", "ggplot2", "glue", "httr2", "jsonlite",
  "purrr", "RSQLite", "scales", "shiny", "shinyjs", "sodium", "tidyr", "testthat"
))
shiny::runApp()
```

Varsayılan yerel geliştirme girişi `arda / kazanma-lab`'dır. Bu sadece geliştirme kolaylığı içindir.

## Süper Lig veri kapsamı

- Lig üyeliği ve fikstür: TFF 2026-27 resmi fikstürü.
- Geçmiş performans: TFF 2025-26 final puan tablosu (O/G/B/M/A/Y/P).
- Kadro büyüklüğü için zayıf öncül: 2026-27 Transfermarkt takım piyasa değerleri.
- Oyun stili: resmi sonuçlar ve kamuya açık sezon incelemelerinden türetilmiş, 0-100 arası küratörlü uzman puanı.
- Güncellik tarihi: 24 Ağustos 2026.

Kaynak envanteri `data/super_lig_sources.csv` dosyasındadır. Taktik puanlar gözlenmiş tracking verisi gibi sunulmaz; teknik direktör veya büyük kadro değişimi olan takımların profil güveni özellikle düşürülür.

Kocaelispor–Amed oranları kullanıcı tarafından gönderilmiş 24 Ağustos görüntüsüdür; canlı akış değildir. Aynı görüntüde Petković golcü oranı bulunmasına rağmen oyuncunun maçta olmadığı doğrulandığı için bu satır “bayat oran” olarak değerlendirme dışıdır. Korner ve kart bahislerinde kalibre edilmiş olay modeli bulunmadığından bu marketler için olasılık uydurulmaz.

## Güvenli kişisel kullanım

1. `.env.example` dosyasını `.env` olarak kopyala.
2. Güçlü bir parola için hash üret:

```bash
Rscript scripts/make_password_hash.R "uzun-ve-benzersiz-sifren"
```

3. Çıktıyı barındırma servisindeki `APP_PASSWORD_HASH` gizli değişkenine ekle.
4. `APP_ENV=production` ve `APP_USERNAME=<kullanıcı-adın>` ayarla.
5. `APP_PASSWORD` kullanma; düz metin şifreyi GitHub'a hiçbir zaman gönderme.
6. Uygulamayı yalnızca HTTPS üzerinden aç.

Uygulama şifre kapılıdır; kaynak kod deposu da GitHub'da **private** oluşturulmalıdır. Daha güçlü ikinci katman için Cloudflare Access, Tailscale veya barındırma sağlayıcısının kimlik doğrulamasını ekleyebilirsin.

## Docker ile çalıştırma

```bash
cp .env.example .env
docker compose up --build
```

Uygulama `http://localhost:3838` adresinde açılır. `kazanma-data` volume'u analiz hafızasını yeniden başlatmalar arasında korur.

## Canlı veri bağlantısı

`.env` veya barındırma servisinin gizli ayarlarında:

```text
FOOTBALL_API_KEY=...
FOOTBALL_TIMEZONE=Europe/Istanbul
```

Lig kimliği `203`, sezon da `2026` olarak kodda kilitlidir; ortam değişkeniyle başka lige çevrilemez.

API-Football'da bir fixture ID; fikstür, olay, istatistik, oyuncu performansı, ilk 11, sakatlık ve tahmin verilerini bağlayan ana anahtardır. Resmi ilk 11 çoğu organizasyonda maçtan kısa süre önce gelir; veri yoksa uygulama tahmin durumunu “onaylı” gibi göstermemelidir.

Kaynaklar:

- [API-Football başlangıç ve endpoint rehberi](https://www.api-football.com/news/post/how-to-get-started-with-api-football-the-complete-beginners-guide)
- [API-Football kota ve coverage rehberi](https://www.api-football.com/news/post/how-to-optimize-api-sports-calls-and-quota-usage)
- [API-Football fiyatlandırma](https://www.api-football.com/pricing)

## Maç-sonu öğrenme döngüsü

Sonucu doğrudan **Model hafızası** ekranından girebilir veya `data/postmatch_template.csv` biçiminde toplu yükleyebilirsin. Zorunlu alanlar:

```text
fixture_id, match_date, home_team, away_team, home_goals, away_goals
```

Önerilen ek alanlar:

```text
home_xg, away_xg, home_cards, away_cards
```

Aynı `fixture_id` tekrar yüklenirse kayıt güncellenir. Tahminler analiz anında dondurulur. Model sağlık kartı yalnızca maç başlangıcından önce kaydedilen son tahmini kullanır; sonradan üretilen tahminler doğruluğu yapay biçimde yükseltemez.

## Model yol haritası

MVP sonrası üretim sırası:

1. Süper Lig için 2–3 sezonluk fikstür, takım, oyuncu, sakatlık ve maç istatistiği senkronizasyonu.
2. Takım hücum/savunma gücü için zaman ağırlıklı Dixon–Coles veya hiyerarşik Poisson model.
3. İlk 11 için oyuncu uygunluğu, son başlangıçlar, teknik direktör formasyonu ve pozisyon rekabetini kullanan sınıflandırıcı.
4. Golcü ve kart için dakika koşullu ayrı oyuncu modelleri.
5. Rolling-origin backtest; Brier, log loss, calibration error ve top-k lineup accuracy.
6. Model registry, veri sürümü ve otomatik maç-sonu yeniden eğitim görevi.
7. Açıklanabilirlik: her tahminde en çok etkileyen 5 sinyal ve son dakika değişikliği.

## GitHub'a gönderme

Bu klasörde yerel Git geçmişini başlattıktan sonra GitHub'da boş ve **private** bir depo oluştur:

```bash
git remote add origin <private-repo-adresi>
git push -u origin main
```

`.env`, API anahtarı, parola ve SQLite veritabanı `.gitignore` kapsamındadır.

## Sorumlu kullanım

Olasılık, kesinlik değildir. Özellikle kadro açıklanmadan önce oyuncu pazarları yüksek belirsizlik taşır. Model ölçümleri yeterli gerçek maç örneği olmadan yayınlanmamalı; düşük kapsamlı liglerde veri boşlukları açıkça gösterilmelidir. Bu yazılım bahis tavsiyesi değildir.
