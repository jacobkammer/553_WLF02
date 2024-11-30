
#Create ATB database ####
antibiotic_db <- dbConnect(RSQLite::SQLite(), "ATB.db")

#Create table of bacteria ####
dbExecute(antibiotic_db,
          "CREATE TABLE Bacteria(
            bacteria varchar(10) NOT NULL PRIMARY KEY,
          gram_stain varchar(10),
          gene_target varchar(10),
          gene_mutation varchar(15),
          serial_num real);") 

#create table detailing microbial fitness ####
dbExecute(antibiotic_db,
          "CREATE TABLE microbial_fitness (
          fitness varchar(10) NOT NULL PRIMARY KEY,
          MIC real,
          Selection_coefficient real,
          SE real,
          gene_mutation,
          FOREIGN KEY (gene_mutation) REFERENCES Bacteria (gene_mutation),
          FOREIGN KEY (antibiotic) REFERENCES Antibiotic(antibiotic));")

# create table of antibitoics and the respective calss ####
dbExecute(antibiotic_db,
          "CREATE TABLE Antibiotic_Table(
          antibioticType varchar(20) NOT NULL PRIMARY KEY,
          antibioticClass varchar(20));")


#create table for lab standards
dbExecute(antibiotic_db,
          "CREATE TABLE Laboratory_standard(
          strain_reference varchar(25),
          technique varchar(20),
          medium varchar(20),
          serial_num real,
          FOREIGN KEY (serial_num) REFERENCES Bacteria (serial_num));")
