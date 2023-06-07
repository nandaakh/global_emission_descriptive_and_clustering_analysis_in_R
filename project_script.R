library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(cluster)

df <- read.csv("global_emissions.csv")

# Data Pre-processing
df <- na.omit(df)
df <- distinct(df)
colnames(df) <- c(
  "year", "country", "country_code", "country_gdp", "population",
  "ch4_prod", "n2o_prod", "co2_cement_prod", "co2_coal_prod",
  "co2_gas_prod", "co2_oil_prod", "co2_flare_prod", "co2_other_prod",
  "total_prod", "co2_cement_global", "co2_coal_global", "co2_gas_global",
  "co2_oil_global", "co2_flare_global", "total_global"
)

head(df)

# cek tipe data masing-masing atribut
column_types <- sapply(df, class)
print(column_types)
# mengubah kolom tahun menjadi tipe data string
df$year <- as.character(df$year)
emission_df <- sapply(df, class)
print(emission_df)

emission_df <- df

# Summarize data untuk melihat rata-rata dan median serta parameter lain
summary(emission_df)

# Analyze Process
# Q1: Bagaimana tren emisi global CH4, N2O, dan CO2 dari tahun ke tahun?
# menggabungkan atau menjumlahkan nilai total CO2 
co_total <- emission_df %>%
  mutate(co2_total_emission = total_prod + total_global)
head(co_total)

# Mengelompokkan data berdasarkan tahun dan menghitung total emisi CH4, N2O, dan CO2
yearly_emission <- co_total %>% 
  group_by(year) %>% 
  summarize(Total_CH4 = sum(ch4_prod),
            Total_N2O = sum(n2o_prod),
            Total_CO2 = sum(co2_total_emission))
# Visualisasi tren emisi dengan membuat plot garis untuk tren emisi CH4, N2O, dan CO2
ggplot(data = yearly_emission, aes(x = year, group = 1)) + # menambahkan angka 1 pada estetika agar ggplot menganggap data adalah satu kelompok
  geom_line(aes(y = Total_CH4, color = "CH4"), linetype = "solid") +
  geom_line(aes(y = Total_N2O, color = "N2O"), linetype = "dashed") +
  geom_line(aes(y = Total_CO2, color = "CO2"), linetype = "dotted") +
  xlab("Tahun") +
  ylab("Total Emisi") +
  scale_color_manual(values = c("CH4" = "blue", "N2O" = "red", "CO2" = "green"),
                     labels = c("CH4", "N2O", "CO2")) +
  theme_minimal()

# Q2: Negara mana yang memiliki emisi CO2 tertinggi? Bagaimana perbandingannya dengan GDP dan populasi negara tersebut?
# Menghitung total emisi CO2, gdp, dan populasi per negara
co2_by_country <- co_total %>%
  group_by(country) %>%
  summarise(total_co2 = sum(co2_total_emission)) %>%
  arrange(desc(total_co2))

gdp_by_country <- emission_df %>%
  group_by(country) %>%
  summarise(total_gdp = sum(country_gdp)) %>%
  arrange(desc(total_gdp))

population_by_country <- emission_df %>%
  group_by(country) %>%
  summarise(total_population = sum(population)) %>%
  arrange(desc(total_population))

# Menggabungkan data emisi CO2 dengan data GDP dan populasi
merged_data <- merge(co2_by_country, gdp_by_country, by = "country", all.x = TRUE) %>%
  merge(population_by_country, by = "country", all.x = TRUE)

# Mencari negara dengan emisi CO2 tertinggi
highest_co_country <- merged_data %>%
  filter(total_co2 == max(total_co2))
# Insight yang dapat diambil dari perbandingan ini adalah sebagai berikut:

#China memiliki emisi CO2 yang sangat tinggi dibandingkan dengan negara-negara lain yang terdapat dalam dataset tersebut. Hal ini menunjukkan kontribusi yang signifikan dari China terhadap emisi CO2 global.
#GDP China yang tinggi (tertinggi ke-2 setelah Amerika Serikat) menunjukkan bahwa negara ini memiliki ekonomi yang kuat dan besar. Hubungan antara GDP dan emisi CO2 perlu dipertimbangkan untuk memahami dampak aktivitas ekonomi terhadap lingkungan.
#Dengan populasi yang sangat besar, China memiliki tantangan tersendiri dalam mengelola emisi CO2 dan mempertahankan pertumbuhan ekonomi yang berkelanjutan. Perhatian terhadap efisiensi energi, sumber energi terbarukan, dan kebijakan lingkungan yang efektif sangat penting bagi negara dengan populasi sebesar China.

# Q3: Apakah ada hubungan antara pertumbuhan GDP suatu negara dengan emisi CO2?
# Menggabungkan data GDP dan emisi CO2
gdp_x_co2 <- merge(gdp_by_country, co2_by_country, by = "country", all.x = TRUE)

top_10_countries <- gdp_x_co2 %>%
  arrange(desc(total_gdp)) %>%
  head(10)

# Membuat scatter plot
ggplot(gdp_x_co2, aes(x = total_gdp, y = total_co2)) +
  geom_point(color = "steelblue") +
  labs(title = "Hubungan antara GDP dan Emisi CO2",
       x = "GDP",
       y = "Emisi CO2") +
  theme_minimal()

# Dari hasil analisa diatas didapatkan bahwa terjadi hubungan positif yang lemah, yang menandakan bahwa kenaikan GDP suatu negara tidak terlalu berpengaruh terhadap tingginya emisi CO2
# Bagaimana hubungan dengan emisi CH4
ch4_by_country <- co_total %>%
  group_by(country) %>%
  summarise(total_ch4 = sum(ch4_prod)) %>%
  arrange(desc(total_ch4))
gdp_x_ch4 <- merge(gdp_by_country, ch4_by_country, by = "country", all.x = TRUE)
# Membuat scatter plot
ggplot(gdp_x_ch4, aes(x = total_gdp, y = total_ch4)) +
  geom_point(color = "steelblue") +
  labs(title = "Hubungan antara GDP dan Emisi CH4",
       x = "GDP",
       y = "Emisi CH4") +
  theme_minimal()
# Hasil yang didapatkan juga sama, bahwa kenaikan GDP tidak terlalu mempengaruhi terjadap tingginya emisi

# Q4: Bagaimanakah hubungan yang ditunjukkan antara populasi dari negara dengan emisi CO2?
# Mmebuat dataframe berisikan total populasi tiap negara dengan total emisi CO2
population_x_co2 <- merge(population_by_country, co2_by_country, by = "country", all.x = TRUE)
# Membuat scatter plot untuk mengetahui polanya
ggplot(population_x_co2, aes(x = total_population, y = total_co2)) +
  geom_point(color = "steelblue") +
  labs(title = "Hubungan antara Jumlah Populasi dan Emisi CO2",
       x = "Jumlah Populasi",
       y = "Emisi CO2") +
  theme_minimal()
# Sejauh yang dapat kami perkirakan jumlah populasi tidak terlalu berkontribusi terhadap tingginya emisi CO2 suatu negara, hal itu ditunjukkan dengan adanya hubungan positif yang lemah

# Q5: Apa kontribusi masing-masing sumber emisi CO2 (seperti batu bara, minyak, gas, pembakaran) terhadap total emisi global?
# Mengelompokkan masing-masing kategori dan menjumlahkan value masing-masing
cement <- emission_df %>%
  mutate(total_cement = co2_cement_prod + co2_cement_global)
coal <- emission_df %>%
  mutate(total_coal = co2_coal_prod + co2_coal_global)
gas <- emission_df %>%
  mutate(total_gas = co2_gas_prod + co2_gas_global)
oil <- emission_df %>%
  mutate(total_oil = co2_oil_prod + co2_oil_global)
flaring <- emission_df %>%
  mutate(total_flaring = co2_flare_prod + co2_flare_global)

# Menggabungkan tiap-tiap kategori kedalam dataframe
data_cement <- cement$total_cement
data_coal <- coal$total_coal
data_gas <- gas$total_gas
data_oil <- oil$total_oil
data_flaring <- flaring$total_flaring

# Menghitung total emisi untuk masing-masing sumber
total_emission <- data_cement + data_coal + data_gas + data_oil + data_flaring

# Membuat dataframe baru dengan kontribusi masing-masing sumber emisi
emission_contributions <- data.frame(source = c("Cement", "Flaring", "Coal", "Oil", "Gas"),
                                     emission = c(data_cement, data_flaring, data_coal, data_oil, data_gas),
                                     total_emission = total_emission)

# Menghitung persentase kontribusi masing-masing sumber emisi
emission_contributions <- emission_contributions %>%
  mutate(percentage = emission / sum(emission) * 100)

# Visualisasikan kedalam stacked bar
bar_chart <- ggplot(emission_contributions, aes(x = source, y = emission, fill = source)) +
  geom_bar(stat = "identity") +
  labs(title = "Kontribusi Sumber Emisi CO2 terhadap Total Emisi",
       x = "Sumber Emisi",
       y = "Emisi CO2",
       fill = "Sumber Emisi") +
  theme_minimal()
print(bar_chart)

# Hasil menunjukkan bahwa semen atau pengolahan semen menjadi sumber tertinggi yang berkontribusi terhadap total emisi CO2 (dihitung secara total), diikuti sumber Gas dan pembakaran (Flaring).

# Q6: Bagaimana perbandingan emisi CO2 per kapita antara negara-negara?
# Menghitung emisi CO2 per kapita
co2_population <- population_x_co2 %>%
  mutate(co2_per_capita = total_co2 / total_population)

# Menampilkan hasil perbandingan emisi CO2 per kapita
co2_population <- co2_population %>%
  select(country, co2_per_capita)

# Mengurutkan data berdasarkan emisi CO2 per kapita
co2_population <- co2_population %>%
  arrange(desc(co2_per_capita))

# Menampilkan tabel perbandingan emisi CO2 per kapita
print(co2_population)

# Membuat visualisasi bar chart
bar_chart <- ggplot(co2_population, aes(x = country, y = co2_per_capita)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Perbandingan Emisi CO2 per Kapita antara Negara-negara",
       x = "Negara",
       y = "Emisi CO2 per Kapita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_chart)
# Negara-negara timur tengah mendominasi urutan ini, dengan Qatar berada diposisi pertama. 

# Q7: Apakah ada perbedaan dalam pola emisi antara kelompok negara yang berbeda berdasarkan analisis klaster?
# Untuk menjawab pertanyaan ini kami akan melakukan analisis klaster menggunakan K-means untuk mendapatkan pendekatan terhadap data yang disediakan
# Menggabungkan data CO2 dari sumber-sumber yang berbeda menjadi satu dataframe
co2_data <- bind_cols(data_cement, data_coal, data_gas, data_oil, data_flaring)

# Standarisasi data, untuk memastikan bahwa setiap variabel memiliki skala yang serupa.
co2_data_scaled <- scale(co2_data)

# Menerapkan analisis klaster (contoh menggunakan metode K-means dengan 3 kelompok)
kmeans_model <- kmeans(co2_data_scaled, centers = 3)

# Menambahkan informasi kelompok ke dataframe asli
co2_data_clustered <- co2_data %>%
  mutate(cluster = kmeans_model$cluster)

# Melihat hasil klastering
print(co2_data_clustered)

# Visualisasi hasil klastering
plot(co2_data_scaled, col = kmeans_model$cluster, pch = 16)

# Menampilkan pusat klaster
centers <- as.data.frame(scale(kmeans_model$centers))
centers$cluster <- 1:3 #length(kmeans_model$centers)
print(centers)

# kami memerlukan satu upaya untuk membandingkan koordinat pusat antara kluster dan melihat perbedaan relatif dalam karakteristik
centers_long <- gather(centers, key = "feature", value = "value", -cluster)

# Visualkan temuan untuk pemahaman lebih lanjut
ggplot(centers_long, aes(x = feature, y = value, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Fitur") +
  ylab("Koordinat Pusat") +
  scale_fill_discrete(name = "Kluster") +
  theme_minimal()
# digunakan diagram batang untuk menunjukkan perbedaan absolut antara koordinat pusat

# Mencoba melihat tren perubahan relatif antara klaster
ggplot(centers_long, aes(x = feature, y = value, group = 1, color = factor(cluster))) +
  geom_line() +
  xlab("Fitur") +
  ylab("Koordinat Pusat") +
  scale_color_discrete(name = "Kluster") +
  theme_minimal()

# Menggabungkan nama negara ke dataframe hasil klastering
co2_data_clustered <- co2_data_clustered %>%
  mutate(country = cement$country)

# Memunculkan nama-nama negara yang akan dikelompokkan berdasarkan klaster
# Mengelompokkan nama negara berdasarkan klaster
clustered_countries <- co2_data_clustered %>%
  select(country, cluster) %>%
  distinct()

# Menampilkan nama-nama negara dalam setiap klaster
for (i in 1:max(kmeans_model$cluster)) {
  cat("Klaster", i, ":", "\n")
  print(clustered_countries$country[clustered_countries$cluster == i])
  cat("\n")
}

