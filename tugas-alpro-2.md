Tugas Alpro 2
================
Renata Subekti
2026-09-28

## Tugas 1 : Mencari Median Tanpa Fungsi Bawaan

**Soal:** Diberikan sebuah vektor numerik, cari mediannya dengan fungsi
yang dibuat sendiri.

**Kode**

``` r
find_median <- function(x) {
  x_sorted <- sort(x) 
  n <- length(x_sorted) 
  
if (n %% 2 == 1) {
  # Jika panjang ganjil, ambil nilai tengah
  return(x_sorted[(n + 1) / 2])
} else {
  # Jika Panjang genap, ambil rata rata dua nilai tengah
  mid1 <- n / 2
  mid2 <- (n / 2) + 1
  return((x_sorted[mid1] + x_sorted[mid2]) / 2)
}
}

# Contoh Penggunaan
x <- c(4, 7, 3, 1, 6)
find_median(x)
```

    ## [1] 4

``` r
x <- c(1, 4, 2, 7)
find_median(x)
```

    ## [1] 3

## Tugas 2 : Mean yang Diperluas (Iterasi)

**Soal:** Diberikan vektor panjang 4, hitung mean, tambahkan ke vektor,
ulangi sampai panjang = 10.

**Kode**

``` r
mean_extend <- function(x){
  cat("Vektor awal:", x, "\n")
  
  # Ulangi selama panjang vektor kurang dari 10
  while(length(x) < 10) {
    m <- sum(x) / length(x)
    x <- c(x, m) 
    cat("Panjang:", length(x), "| Mean", round(m, 4), "\ Vektor:", round(x, 4), "\n")
  }
  
  return(x)
}

# Contoh penggunaan
x <- c(7, 3, 6, 9)
mean_extend(x)
```

    ## Vektor awal: 7 3 6 9 
    ## Panjang: 5 | Mean 6.25  Vektor: 7 3 6 9 6.25 
    ## Panjang: 6 | Mean 6.25  Vektor: 7 3 6 9 6.25 6.25 
    ## Panjang: 7 | Mean 6.25  Vektor: 7 3 6 9 6.25 6.25 6.25 
    ## Panjang: 8 | Mean 6.25  Vektor: 7 3 6 9 6.25 6.25 6.25 6.25 
    ## Panjang: 9 | Mean 6.25  Vektor: 7 3 6 9 6.25 6.25 6.25 6.25 6.25 
    ## Panjang: 10 | Mean 6.25  Vektor: 7 3 6 9 6.25 6.25 6.25 6.25 6.25 6.25

    ##  [1] 7.00 3.00 6.00 9.00 6.25 6.25 6.25 6.25 6.25 6.25

## Tugas 3: Menghitung Jumlah Bilangan Genap

**Soal:** Buat fungsi untuk menghitung jumlah bilangan genap di dalam
vektor.

**Kode**

``` r
hitung_genap <- function(x) {
  return(sum(x %% 2 == 0))
}

# Contoh penggunaan
x <- c(1, 2, 4, 7, 8, 9, 15, 18)
hitung_genap(x)
```

    ## [1] 4

## Tugas 4: Menghitung Jumlah Elemen Genap dalam Matriks

**Soal:** Buat fungsi yang menerima matriks, lalu hitung jumlah semua
elemen genap di dalamnya.

**Kode:**

``` r
hitung_genap_matriks <- function(m) {
  elemen_genap <- m[m %% 2 == 0]
  return(sum(elemen_genap))
}

# Contoh penggunaan
m <- matrix(1:12, nrow = 3, ncol =4)
m
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    4    7   10
    ## [2,]    2    5    8   11
    ## [3,]    3    6    9   12

``` r
hitung_genap_matriks(m)
```

    ## [1] 42
