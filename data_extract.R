library(tabulizer)

# this extract_tables function from tabulizer package extracts data tables from a pdf file
bd<- extract_tables("https://www.iedcr.gov.bd/website/images/files/nCoV/Case_dist_14_May_upload.pdf", pages = 1, output = "data.frame")
# extract_tables returns a list. just take the data.frame from that list. it's the first element on my machine.
bd<- bd[[1]]

# new names came from map data
new_names<- zilla@data$NAME_2

# dist_names is from iedcr data
dist_names<- bd$District.City

# remove the blank names
dist_names<- dist_names[-which(dist_names=="")]
length(dist_names)

# delete "dhaka city" from district names and add just "dhaka"
dist_names[2]<- "Dhaka"
dist_names<- dist_names[-1]
head(dist_names)
length(dist_names)

# district name spellings from map shape file and IEDCR data are not same.
# this causes a little problem.
# we have to find out the diffrent spellings
nameChecker<- logical(length(new_names))

for (i in 1:length(new_names)) {
    if (sum(new_names[i]==dist_names)==1) {
        nameChecker[i]=T
    } else {
        nameChecker[i]=F
    }
}

# let's see which spellings are not same!
missing<- new_names[!nameChecker]
missing

# Now Consider the names from IEDCR data as wrong.
# dont worry we will replace these names with the names from shape file data.
wrong<- c("Barishal", "Jhalokathi", "Potuakhali", "B. Baria", "Chattogram", "Cumilla", "Cox’s bazar", 
          "Khagrachari", "Laksmipur", "Rangmati", "Munshigonj", "Narshingdi", "Netrokona", 
          "Chapainawabganj", "Panchagar", "Hobiganj", "Moulovi Bazar")

# replacing the names.
for (i in 1:length(wrong)) {
    dist_names[which(dist_names==wrong[i])]<- missing[i]
}

# check missing names again just to be sure!

# now create a data frame with district names. and ID to match with Shape file ID's
id_data<- data.frame(id=0:63, district= new_names)

# extract the number of confirmed cases.
dist_cases<- bd$Total

# remove Na's
dist_cases<- dist_cases[-which(is.na(dist_cases))]

# add "dhaka city" value and "dhaka" value in one value.
dist_cases[2]<- dist_cases[2]+ dist_cases[1]
# then delete the "dhaka city" value
dist_cases<- dist_cases[-1]
length(dist_cases)

# add these values to the data frame
for (i in 1:length(dist_names)) {
    id_data$confirmed_cases[i] <- dist_cases[which(dist_names==id_data$district[i])]
}
################ CAUTION !!!!!!!!!!!!!!
########### DO NOT RUN THIS LINE EVERY TIME
########### Run this Line of code for the first time only, to create the CSV file on your Google Sheets account.
ss<- gs4_create("dist_data", sheets = id_data)
######## after creating the csv file change the sharing option to "Anyone can Edit"

# use this line of code to update the data
sheet_write(id_data, ss, sheet = "id_data")



# everything is same as before. just follow these steps to extract the Divisionwise Data.
# DIVISION
new_div<- div@data$NAME_1

Div_names<- bd$Division
Div_names<- Div_names[-which(Div_names=="")]
Div_names<- Div_names[-6]
length(Div_names)
head(Div_names)

DIVnameChecker<- logical(length(new_div))

for (i in 1:length(new_div)) {
    if (sum(new_div[i]==Div_names)==1) {
        DIVnameChecker[i]=T
    } else {
        DIVnameChecker[i]=F
    }
}

missing_div<- new_div[!DIVnameChecker]
missing_div
wrong_div<- c("Barishal", "Chattogram")

for (i in 1:length(wrong_div)) {
    Div_names[which(Div_names==wrong_div[i])]<- missing_div[i]
}

div_data<- data.frame(id=0:6, Division= new_div)

div_cases<- bd$Division.1
div_cases<- div_cases[-which(is.na(div_cases))]
div_cases[2]<- div_cases[2]+ div_cases[1]+ div_cases[7]
div_cases<- div_cases[-c(1, 7)]
length(div_cases)

for (i in 1:length(Div_names)) {
    div_data$confirmed_cases[i] <- div_cases[which(Div_names==div_data$Division[i])]
}

ss_2<- gs4_create("div_data", sheets = div_data)

sheet_write(div_data, ss_2, sheet = "div_data")
