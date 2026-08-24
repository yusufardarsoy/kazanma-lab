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
- API-Football resmî API'sinden fikstür, resmî ilk 11, sakatlık/ceza ve maç-sonu veri bağlantısı
- Football-Data.co.uk'nun anahtar gerektirmeyen ücretsiz CSV'lerinden yayınlanan maç günü/saatı, sonuç, xG/şut/kart istatistiği ve piyasa ortalaması oranları
- The Odds API'nin opsiyonel 500 kredi/ay ücretsiz planından güncel 1X2, 2,5 gol oranları ve son üç günlük tamamlanmış skorlar
- Site kapalıyken çalışan Windows görevi; açılışta geriye dönük eksik tamamlama
- Aynı maçı çoğaltmayan SQLite kayıtları, sonuç kaynak geçmişi ve ücretsiz kotalar için 24 saatlik/8 saatlik güvenlik frenleri
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

Kurulum tamamlandıktan sonra en kolay yol proje kökündeki **`Kazanma-Lab-Ac.cmd`** dosyasına çift tıklamaktır. Başlatıcı önce kaçırılan verileri tamamlamayı dener, sonra siteyi `http://127.0.0.1:3838` adresinde açar. Siteyi durdurmak için açılan siyah pencerede `Ctrl+C` kullan.

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

## Ücretsiz veri bağlantıları

Temel akış API anahtarı ve ödeme istemez. Uygulama [Football-Data.co.uk Türkiye CSV'sini](https://www.football-data.co.uk/turkeym.php) ve [yayınlanmış fikstür CSV'sini](https://www.football-data.co.uk/matches.php) doğrudan indirir. Sağlayıcı veriyi ücretsiz olarak yayımlar ve sonuç dosyalarını en az haftada iki kez güncellediğini belirtir. Bu nedenle anahtarsız sonuç akışı gerçek zamanlı değildir; kaynak yeni dosyayı yayımladığında görev geriye dönük tamamlar. İngiltere yerel saatleri `Europe/London` zaman diliminden `Europe/Istanbul` zaman dilimine çevrilir. Takım adlarının en az %90'ı ev/deplasman yönüyle TFF kataloğuna eşleşmezse içe aktarma durur.

Güncel oran ve daha hızlı sonuç için tek tek Misli verisi girmek gerekmez. Ana kaynak olarak [API-Football](https://www.api-football.com/pricing) ücretsiz anahtarı yeterlidir: fikstür, sonuç, ilk 11, sakatlık ve maç önü oranlarını aynı bağlantıdan alır. Motor 1X2 ve 2,5 gol marketlerini maç tarihine göre toplu çeker, en fazla üç saatte bir yeniler ve aynı günün tüm maçlarını tek çağrıda isteyerek ücretsiz kotayı korur.

[The Odds API](https://the-odds-api.com/) ikinci ve isteğe bağlı oran kaynağıdır. Ücretsiz Starter planı 500 kredi/aydır; sağlayıcının kapsam listesinde Türkiye Süper Ligi `soccer_turkey_super_league` anahtarıyla oran ve skor desteği vardır. Motor bu kaynağı eklediğinde 1X2 ve 2,5 gol marketlerini en fazla 8 saatte bir yeniler, yanıt başlıklarındaki kalan krediyi kaydeder ve ücretsiz aylık bütçenin altında kalacak şekilde fren uygular. Ham veriyi ayrı bir ürün gibi yeniden satmak/dağıtmak yasaktır.

API-Football ücretsiz planda ödeme kartı istemeden 100 istek/gün verir; kapsam endpoint'i ilgili sezon için bir veri türünü onaylamazsa motor o endpoint'i çekmez. Maç önü oranları sağlayıcıda genellikle maçtan 1–14 gün önce görünür ve yaklaşık üç saatte bir güncellenir. Kullanım koşullarına ve yerel mevzuata uymak kullanıcı sorumluluğundadır; bu açıklama hukuki görüş değildir.

1. [API-Football](https://dashboard.api-football.com/register) üzerinden ücretsiz hesap açıp anahtarı al.
2. `.env.example` dosyasını `.env` adıyla kopyala.
3. `.env` içindeki `FOOTBALL_API_KEY=` satırının sonuna anahtarı yaz. `.env` ve SQLite dosyası GitHub'a gönderilmez.

`.env` veya barındırma servisinin gizli ayarlarında:

```text
FOOTBALL_API_KEY=...
FOOTBALL_TIMEZONE=Europe/Istanbul
ODDS_API_KEY=istege-bagli-ucretsiz-anahtar
```

Lig kimliği `203`, sezon da `2026` olarak kodda kilitlidir; ortam değişkeniyle başka lige çevrilemez. Motor her turda önce bütün sezon fikstürünü tek çağrıyla uzlaştırır. Yaklaşan maçlarda eksikleri, başlama saatine 90 dakika kala resmî ilk 11'i; biten maçlarda skor, olay, takım ve oyuncu istatistiklerini kademeli olarak saklar.

API-Football'da bir fixture ID; fikstür, olay, istatistik, oyuncu performansı, ilk 11 ve sakatlık verilerini bağlayan ana anahtardır. Sağlayıcı ID'si TFF ID'si sanılmaz; ev/deplasman takımı ve hafta üzerinden iç fikstürde tam bir eşleşme aranır. Eşleşme oranı `%90` altına düşerse hatalı kayıt riskine karşı senkronizasyon durur. Resmî ilk 11 çoğu organizasyonda maçtan 20–40 dakika önce gelir; veri yoksa uygulama “onaylı” göstermez.

Kaynaklar:

- [Football-Data.co.uk veri sayfası ve güncelleme düzeni](https://www.football-data.co.uk/data)
- [Football-Data.co.uk Türkiye CSV'si](https://www.football-data.co.uk/turkeym.php)
- [The Odds API fiyatlandırma ve kapsam](https://the-odds-api.com/)
- [The Odds API v4 kullanım rehberi](https://the-odds-api.com/liveapi/guides/v4/)
- [The Odds API kullanım koşulları](https://the-odds-api.com/terms-and-conditions.html)
- [API-Football başlangıç ve endpoint rehberi](https://www.api-football.com/news/post/how-to-get-started-with-api-football-the-complete-beginners-guide)
- [API-Football kota ve coverage rehberi](https://www.api-football.com/news/post/how-to-optimize-api-sports-calls-and-quota-usage)
- [API-Football fiyatlandırma](https://www.api-football.com/pricing)
- [API-Football kullanım koşulları](https://www.api-football.com/terms)

## Site kapalıyken otomatik kayıt

PowerShell'de proje klasöründeyken bir kez şunu çalıştır; anahtarsız Football-Data akışı için `.env` zorunlu değildir:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\scripts\Install-Windows-Data-Task.ps1
```

`KazanmaLabDataSync` adlı görev şunları yapar:

- Windows oturumu açıldığında bir geriye-dönük tamamlama çalıştırır.
- Her gün 10:00'da sonuç/fikstür/oran kontrolü yapar.
- Maç saatlerini kapsamak için 16:00–00:00 arasında 30 dakikada bir çalışır.
- Site açık olmasa da `data/kazanma.sqlite` içine sonuçları, kaynak bilgisini, oran görüntülerini ve varsa ham ayrıntı paketlerini yazar.
- Bilgisayar kapalı veya uykudaysa o anda veri çekemez; Windows yeniden kullanılabilir olduğunda kaçırılan maçları sezon fikstüründen geriye dönük tamamlar.

Görev kayıtları `data/cache/sync.log` dosyasındadır. SQLite ve önbellek uygulama yeniden başlasa da kalır, ancak özellikle GitHub'a gönderilmez. Kişisel yedek almak istersen site ve görev kapalıyken `data/kazanma.sqlite` dosyasını güvenli bir diske kopyala.

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
