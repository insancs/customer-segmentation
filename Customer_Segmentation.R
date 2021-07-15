library(ggplot2)
pelanggan <- read.csv("E:/DQLab/Dataset/customer_segments.txt", sep = "\t")

#---------Main Concept-----------
#Konversi data menjadi numerik 
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])
pelanggan$NilaiBelanjaSetahun <- as.integer(pelanggan$NilaiBelanjaSetahun)

#Menggabungkan data pelanggan dengan pelanggan_matrix
pelanggan <- data.frame(pelanggan, pelanggan_matrix)

#Membuat data master
Profesi <- unique(pelanggan[c("Profesi", "Profesi.1")])
Jenis_Kelamin <- unique(pelanggan[c("Jenis.Kelamin", "Jenis.Kelamin.1")])
Tipe_Residen <- unique(pelanggan[c("Tipe.Residen", "Tipe.Residen.1")])
#Normalisasi kolon NilaiBelanjaSetahun menjadi puluhan 
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun / 1000000 
field_yang_digunakan <- pelanggan[c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1", "NilaiBelanjaSetahun")]
field_yang_digunakan2 <- c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1", "NilaiBelanjaSetahun")
#Bagian K-Means
set.seed(100)
#fungsi kmeans untuk membentuk 5 cluster dengan 25 skenario random dan simpan ke dalam variable segmentasi
segmentasi <- kmeans(x=field_yang_digunakan, centers = 5, nstart = 25)

#Menambahkan kolom cluster dari kmeans ke data pelanggan 
pelanggan$cluster <- segmentasi$cluster
#---------Analis Hasil Cluster----------
# #Filter cluster ke-1
# which(pelanggan$cluster == 1)
# length(which(pelanggan$cluster == 1))
# #Melihat data cluster ke-1
# pelanggan[which(pelanggan$cluster == 1),]
#---------Elbow Effect------
sse <- sapply(1:10, function(param_k){kmeans(pelanggan[field_yang_digunakan2], param_k, nstart=25)$tot.withinss})
jumlah_cluster_max <- 10
ssdata = data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) +
geom_line(color="red") + geom_point() +
ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") +
geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
scale_x_discrete(limits=c(1:jumlah_cluster_max))

#---------Pemaketan Model K-Means----------
Segmen.Pelanggan <- data.frame(cluster=c(1,2,3,4,5), Nama.Segmen=c("Silver Youth Gals", "Diamond Senior Member", "Gold Young Professional", "Diamond Professional", "Silver Mid Professional"))
Identitas.Cluster <- list(Profesi=Profesi, Jenis.Kelamin=Jenis_Kelamin, Tipe.Residen=Tipe_Residen, Segmentasi=segmentasi, Segmen.Pelanggan=Segmen.Pelanggan, field_yang_digunakan=field_yang_digunakan)
Identitas.Cluster
#Menyimpan objek ke dalam bentuk file
saveRDS(Identitas.Cluster, "cluster.rds")

#---------Data Baru-------------
databaru <- data.frame(Customer_ID = "CUST-100", Nama.Pelanggan = "Rudi Wilamar", Umur = 20, Jenis.Kelamin = "Wanita", Profesi = "Pelajar", Tipe.Residen = "Cluster", NilaiBelanjaSetahun=3.5)
str(databaru)
#Membaca data readRDS
Identitas_Cluster <- readRDS(file="C:/Users/insancs/Documents/cluster.rds")
#Masukkan perintah untuk penggabungan data
databaru <- merge(databaru, Identitas_Cluster$Profesi)
databaru <- merge(databaru, Identitas_Cluster$Jenis.Kelamin)
databaru <- merge(databaru, Identitas_Cluster$Tipe.Residen)
databaru
#menentukan data baru di cluster mana
which.min(sapply(1:5, function( x ) sum( (databaru[Identitas_Cluster$field_yang_digunakan] - Identitas_Cluster$Segmentasi$centers[x,])^2 ) ))
Identitas_Cluster$Segmen.Pelanggan[which.min(sapply( 1:5, function( x ) sum( ( databaru[Identitas_Cluster$field_yang_digunakan] - Identitas_Cluster$Segmentasi$centers[x,])^2 ) )),]
