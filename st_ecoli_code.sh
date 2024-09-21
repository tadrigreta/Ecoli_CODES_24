library(tidyverse)

st=read.csv('all.csv')

colnames(st)
fdat=st |> select(Run, AvgSpotLen, Bases, BioProject, BioSample, geo_loc_name_country, geo_loc_name_country_continent, Host, isolation_source, Host_disease, host_sex) 

fdat$Host <- trimws(tolower(fdat$Host))

fdat$Host|> unique()

fdat = fdat |>
  mutate(corr_host = case_when(
    Host %in% c("pig", "swine", "sus scrofa domesticus", "sus scrofa") ~ "Pig",
    Host %in% c("bovine", "cattle","bos taurus") ~ "Cattle",
    Host %in% c("human", "homo sapiens", "homo_sapiens", "homo sapiens sapiens") ~ "Human", 
    Host %in% c("turkey") ~ "Turkey", 
    Host %in% c("chicken", "gallus gallus domesticus", "gallus gallus domesticuss") ~ "Chicken", 
    Host %in% c("poultry animal (variety unknown)") ~ "Unknown poultry",
    Host %in% c("bird") ~ "Bird",
    Host %in% c("bucephala") ~ "Bird (Bucephala)", 
    Host %in% c("rat") ~ "Rat", 
    Host %in% c("horse", "equus caballus") ~ "Horse", 
    Host %in% c("goat", "ovis aries") ~ "Goat", 
    Host %in% c("cat", "felis catus") ~ "Cat", 
    Host %in% c("dog", "canis lupus familiaris") ~ "Dog", 
    Host %in% c("milk") ~ "Milk", 
    Host %in% c("human/food") ~ "Food",
    Host %in% c("environment") ~ "Environment", 
    Host %in% c("", "not applicable", "missing", NA) ~ "Unknown", 
    Host %in% c("pteropus poliocephalus") ~ "Flying fox",
    Host %in% c("oryctolagus cuniculus") ~ "European rabbit",
    TRUE ~ Host
  ))


fdat$isolation_source <- trimws(tolower(fdat$isolation_source))

fdat$isolation_source|> unique()

fidat = fdat |>
  mutate(corr_host = case_when(
    isolation_source %in% c("treated effluent water collected at a wastewater treatment plant", "water from hjalmaren lake", "water from svartan river","pond water collected in a bird sanctuary","incoming wastewater from a wastewater treatment plant\\, after removal of large particulate matter", "momutu stream") ~ "Environment",
    isolation_source %in% c("cloacal swabs from pigeons") ~ "Pigeon",
    isolation_source %in% c("feline") ~ "Cat",
    isolation_source %in% c("horse") ~ "Horse",
    isolation_source %in% c("human","Human fecal sample", "clinical sample", "Human clinical faeces") ~ "Human",
    isolation_source %in% c("food", "pork","chicken breast", "porcine meat", "beef", "chicken giblets","chicken drumstick", "chicken meat spiced","chicken schnitzel", "pork belly", "turkey breast", "pork meat raw","chicken meat raw","chicken meat strips","chicken wings", "turkey drumstick", "chicken meat","turkey schnitzel", "pork mincemeat") ~ "Food",
    isolation_source %in% c("swine", "diseased pigs") ~ "Pig",
    isolation_source %in% c("poultry") ~ "Unknown poultry",
    TRUE ~ corr_host))

fdat$isolation_source <- trimws(tolower(fdat$isolation_source))

fdat$isolation_source|> unique()
   
fiidat = fidat |>
    mutate(corr_isol_source = case_when(
      isolation_source %in% c("urine", "urine specimen") ~ "Urine",
      isolation_source %in% c("rectal swab") ~ "Rectal swab",
      isolation_source %in% c("anal gland sinus") ~ "Anal gland",
      isolation_source %in% c("stomach wall swab") ~ "Stomach wall swab",
      isolation_source %in% c("blood", "blood culture", "bloodstream") ~ "Blood",
      isolation_source %in% c("", "not applicable", "missing", "u cystoscopy", "lab generated","not collected", "laboratory", "NA", "tissue", "not provided", "cell culture", "swab", "facility k", "facility g", "facility b","facility a", "bacterial isolate specimen", "poultry") ~ "Unknown",
      isolation_source %in% c(NA) ~ "Unknown",  
      isolation_source %in% c("wound") ~ "Unknown infection",
      isolation_source %in% c("feces - environment", 'stool', "feces","enviromental avian faeces in farm","coprocultive", "human fecal sample", "avian feces", "fecal content", "fecal swab","fox fecal swab", "human clinical faeces", "faecal sample", "feacal sample")  ~ "Feces",
      isolation_source %in% c("ear swab") ~ "Ear swab",
      isolation_source %in% c("vaginal discharge") ~ "Vaginal swab",
      isolation_source %in% c("sputum") ~ "Sputum",
      isolation_source %in% c("liver abscess", "bile") ~ "Liver-associated",
      isolation_source %in% c("cerebrospinal fluid") ~ "Cerebrospinal fluid",
      isolation_source %in% c("tissue\\, hole in mandibula","bone") ~ "Bone-associated",
      isolation_source %in% c("gut", "screening (rectal)") ~ "Gut",
      isolation_source %in% c("chicken breast", "chicken giblets","chicken drumstick","chicken meat spiced","chicken schnitzel", "turkey breast","chicken meat raw","chicken meat strips", "chicken wings", "turkey drumstick", "chicken meat","turkey schnitzel") ~ "Poultry (meat)",
      isolation_source %in% c("beef") ~ "Beef (meat)",
      isolation_source %in% c("cloacal swab") ~ "Cloacal swab",      
      isolation_source %in% c("wild bird", "avian", "avian pathology\\, internal organs", "avian feces", "enviromental avian faeces in farm", "cloacal swab", "retail poultry") ~ "Unknown",
    isolation_source %in% c("treated effluent water collected at a wastewater treatment plant","incoming wastewater from a wastewater treatment plant\\, after removal of large particulate matter") ~ "Wastewater",
     isolation_source %in% c("water from hjalmaren lake", "water from svartan river","pond water collected in a bird sanctuary","momutu stream") ~ "Other water",
      isolation_source %in% c("environmental", 'boot swab sample', "dust sample", "carcass") ~ "Other environmental",
            isolation_source %in% c("ruijin hosiptal") ~ "Hospital",
    isolation_source %in% c("cloacal swabs from pigeons") ~ "Cloacal swab",
    isolation_source %in% c("feline") ~ "Unknown",
    isolation_source %in% c("horse") ~ "Unknown",
    isolation_source %in% c("human") ~ "Unknown",
    isolation_source %in% c("food") ~ "Unknown",
    isolation_source %in% c("swine") ~ "Unknown",         
      TRUE ~ isolation_source))

fiidat$Host_disease <- trimws(tolower(fdat$Host_disease))

fiidat$Host_disease|> unique()

fiddat = fiidat |>
mutate(
    corr_host_disease = case_when(
    Host_disease %in% c("not applicable", "", "missing", "colonization", "bacterial infectious disease", "not collected","carbapenemase-producing enterobacterales", "restricted access") ~ "Unknown",
    Host_disease %in% c("bacteriuria", "uti", "urinary tract infection", "escherichia coli uti") ~ "UTI",
    Host_disease %in% c("bacteremia") ~ "Bacteremia",
    Host_disease %in% c("cholecystisis") ~ "Cholecystisis",
    Host_disease %in% c("hopsital aqcuired infection") ~ "Nosocomial",
    TRUE ~ Host_disease
  ))

fiddat = fiddat |>
mutate(
    geo_loc_name_country = case_when(
    geo_loc_name_country %in% c("") ~ "United Kingdom",
    TRUE ~ geo_loc_name_country
  ))
  
fiddat = fiddat %>%
mutate(
    geo_loc_name_country_continent = case_when(
    geo_loc_name_country_continent %in% c("") ~ "Europe",
    TRUE ~ geo_loc_name_country_continent
  ))

write.csv(x=fiddat, file='new_metadata.csv')

fiddat |> group_by(geo_loc_name_country) |> summarize(db=n()) |> arrange(desc(db))
fiddat |> group_by(geo_loc_name_country_continent) |> summarize(db=n()) |> arrange(desc(db))

fiddat |> group_by(geo_loc_name_country, corr_host, corr_isol_source) |> summarize(db=n()) |> arrange(desc(db))

fiddat$geo_loc_name_country |> unique()
fiddat$corr_host_disease |> unique()

all=fiddat |> group_by(BioProject, geo_loc_name_country,corr_host, corr_host_disease, corr_isol_source) |> summarize(db=n()) |> arrange(desc(db))





export PATH=/data/tools/sratoolkit.2.11.3-ubuntu64/bin:$PATH
export PATH=/data/tools/TrimGalore:$PATH

cd /data/tadrigreta/vetmeduni_vienna/ecoli_phylo/st38
#time: started: 14:54 - 15:55
declare -a arr=(
SRR6111513
SRR6111562
ERR9709908
ERR1276256
ERR9709035
SRR21951496
ERR9726642
SRR21883712
SRR25176713
SRR7839425
SRR9887352
SRR24782024
ERR9709907
SRR9887359
SRR27977082
SRR8871596
SRR9887367
SRR11871195
DRR515308
ERR9709052
SRR6892685
SRR11871293
SRR27268747
SRR27936540
SRR8871696
SRR13436083
SRR12903845
SRR5936495
SRR8871591
SRR8871840
SRR11871195
SRR12495244
SRR13257569
SRR19165479
SRR26701854
SRR29027587
SRR11822881
SRR15150507
SRR27366269
SRR24437758
SRR7469842
SRR8871765
SRR11871202
SRR10878361
SRR10950766
SRR12381757
SRR18560472
SRR22179305
SRR11871215
)

for s in "${arr[@]}"
do
  prefetch --max-size 1000GB --output-file $s'.sra' $s
done

for f in *.sra
do
  fastq-dump --split-files $f
done


for f in *_1.fastq
do
r=${f/'_1'/'_2'}
of=${f/'_1.fastq'/'_trim_1.fq'}
uf=${f/'_1.fastq'/'_trim_1_un.fq'}
or=${r/'_2.fastq'/'_trim_2.fq'}
ur=${r/'_2.fastq'/'_trim_2_un.fq'}

TRIM='/data/tools/Trimmomatic-0.39/trimmomatic-0.39.jar'

java -jar $TRIM PE $f $r $of $uf $or $ur -threads 9 SLIDINGWINDOW:5:20 MINLEN:50
done

export PATH=/data/tadrigreta/miniforge3/bin:$PATH
source activate spades
cd /data/tadrigreta/vetmeduni_vienna/ecoli_phylo/st38

for f in *_trim_1.fq
do
r=${f/'_1'/'_2'}
uf=${f/'_trim_1.fq'/'_trim_1_un.fq'}
ur=${r/'_trim_2.fq'/'_trim_2_un.fq'}
d=${f/'_trim_1.fq'/''}
spades.py -1 $f -2 $r --pe1-s $uf --pe2-s $ur -t 18 --isolate -o spades_$d
done

mkdir contigs

for f in spades_*/contigs.fasta
do
  a=${f/'spades_'/''}
  b=${a/'/'/'_'}
  cp $f 'contigs/'$b
done

cd /data/tadrigreta/vetmeduni_vienna/ecoli_phylo/st38/contigs

export PATH=/data/tadrigreta/miniforge3/bin:$PATH
source activate prokka

for f in *_contigs.fasta
do
prokka --outdir prokka_${f/'_contigs.fasta'/''} --prefix ${f/'_contigs.fasta'/''} --genus Escherichia --species coli --cpus 30 $f
done

conda deactivate

mkdir gffs

for f in prokka_*/*.gff
do
  cp $f gffs
done

cd /data/tadrigreta/vetmeduni_vienna/ecoli_phylo/st38/contigs/gffs
mkdir panaroo_results
export PATH=/data/tadrigreta/miniforge3/bin:$PATH
source activate panaroo
panaroo -i *.gff -o panaroo_results -t 10 --clean-mode strict --refind-mode strict --refind_prop_match 0.5 --search_radius 5000 -a core --aligner mafft

cd /data/tadrigreta/vetmeduni_vienna/ecoli_phylo/st38/contigs/gffs/panaroo_results

export PATH=/data/tadrigreta/miniforge3/bin:$PATH
source activate snpsites

snp-sites -r -m -v -o st38_snp core_gene_alignment.aln



library(tidyverse)
library(Biostrings)
library(GenomicRanges)
library(ape)
library(ggtree)
library(Cairo)
library(phangorn)
library(RColorBrewer)
library(ggtreeExtra)
library(ggstar)
library(ggnewscale)

aln = readDNAStringSet('st38_snp.snp_sites.aln')
lst = strsplit(names(aln), ' ')
ns = c()
for(e in lst){
  ns = c(ns, e[1])
}
names(aln) = ns

msa = as.DNAbin(aln)

msa.dat = phyDat(msa, type = "DNA", levels = NULL)

set.seed(123)
mt = modelTest(msa.dat, model=c('JC', 'F81', 'K80', 'HKY', 'SYM', 'GTR'),
 G=T, I=T, k=4, control=pml.control(epsilon=1e-08, maxit=3, trace=1), multicore=F
)

mt$Model[mt$BIC==min(mt$BIC)]

dm = dist.ml(msa.dat, model='F81')
msa_nj = NJ(dm)

boot_tree <- function(data) {
  dm <- dist.ml(data, model = "F81")
  NJ(dm)
}

bs = bootstrap.phyDat(msa.dat, boot_tree, bs = 100) 

nj_tree_bs = plotBS(msa_nj, bs)

nj_tree2 = root(nj_tree_bs, outgroup = "ASM694v2", edgelabel = TRUE)

nj_tree2 = drop.tip(nj_tree2, "pseudo_ref")

nj_tree3 = nj_tree2
nj_tree3 = drop.tip(nj_tree3, "ASM694v2")

meta=read.csv('new_metadata.csv')

names(meta)[names(meta) == "Host"] <- "old_host"
names(meta)[names(meta) == "Host_disease"] <- "old_host_disease"
names(meta)[names(meta) == "isolation_source"] <- "old_isol_source"
names(meta)[names(meta) == "geo_loc_name_country"] <- "Country"
names(meta)[names(meta) == "geo_loc_name_country_continent"] <- "Continent"
names(meta)[names(meta) == "corr_host"] <- "Host"
names(meta)[names(meta) == "corr_host_disease"] <- "Host disease"
names(meta)[names(meta) == "corr_isol_source"] <- "Isolation_source"

colnames(meta)

meta$Country=as.factor(meta$Country)
meta$Host=as.factor(meta$Host)
meta$`Host disease`=as.factor(meta$`Host disease`)
meta$BioProject=as.factor(meta$BioProject)

mt=meta |> select(Run,Country,Host,`Host disease`, BioProject) |> 
#mutate(Run = replace(Run, Run == '257277_SaureusD1SOM', "257277_SA")) |>
#mutate(Run = replace(Run, Run == 'GCF_000009045.1', "GCF000009045")) |>
#mutate(Run = replace(Run, Run == 'GCF_000236925.1', "GCF000236925")) |>
#mutate(Run = replace(Run, Run == 'GCF_024205965.1', "GCF024205965")) |>
mutate(Country = replace(Country, Country == 'Unknown', "NA")) |> 
mutate(Host = replace(Host, Host == 'Unknown', "NA")) |>
mutate(`Host disease` = replace(`Host disease`, `Host disease` == 'Unknown', "NA"))

own=c("ESBL_ID_44", "Austria", "Dog", "AHDS", "In progress")

mt[] <- lapply(mt, as.character)
mt= mt |> rbind(own)
mt[] <- lapply(mt, as.factor)

nj_tree2$node.label <- as.numeric(nj_tree2$node.label)

nj_tree4=nj_tree2

#nj_tree4$node.label[nj_tree4$node.label < 50] <- NA

nj_tree4 |> ggtree(branch.length='none', layout="circular") + geom_nodelab(geom='label') + geom_tiplab(color='black')

nj_tree4 |> ggtree(branch.length='none', layout="circular") + geom_text(aes(label=node), hjust=-.3) + geom_tiplab(color='black')

nj_tree4 |> ggtree(branch.length='none', layout="circular") %<+% mt + 
geom_tiplab(color='black', hjust=-0.05) + geom_tree(aes(color=Host), size=1) + geom_tippoint(aes(shape = `Host disease`), size=3)+ 
scale_shape_manual(values=seq(0,6)) +
    theme(legend.position = "right") + geom_nodelab(geom='label', size=2.5) + 
#geom_hilight(node=97, fill="palegoldenrod") +
#geom_treescale(linesize=0, fontsize=0, width=2) +
     geom_fruit(geom=geom_tile,
         mapping=aes(fill=Country),
         size=1,
         offset=0.3,
         pwidth=0.5
     ) 

nj_tree3 |> ggtree(layout="rectangular") %<+% mt + 
geom_tiplab(color='black', hjust=-0.05) + geom_tree(aes(color=`Host disease`), size=1) + geom_tippoint(aes(shape = Host), size=3)+ 
scale_shape_manual(values=seq(0,6)) +
    theme(legend.position = "right") + geom_nodelab(geom='label', size=2) + 
geom_hilight(node=97, fill="palegoldenrod") +
geom_treescale(linesize=0.5, fontsize=3, width=0.01, y=-1) +
     geom_fruit(geom=geom_tile,
         mapping=aes(fill=Country),
         size=0.01,
         offset=0.25,
         pwidth=0.001
     ) 
