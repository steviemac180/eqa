
classify <- function(x){
  if (x == "Antithrombin" | x == "APCR" | x == "Free Protein S"| x == "Protein C Activity"){
      return("Thrombotic")
      }else{
        if(x == "Anti-Xa"| x == "D-Dimer"| x == "Heparin Ratio"){
        return("Routine")
        }else
          if(x == "vWF Activity" | x == "vWF Antigen"| x == "VWF CBA"| x == "VWF: Rco Activity"){
            return("von Willebrand")
      }else{
      return("Factor")
      }
    }
  }

levels(df.neqas.IQ$Assay)
summary(df.neqas.IQ)

vec.new <- sapply(df.neqas.IQ$Assay, classify)
vec.new

df.neqas.classified <- cbind(df.neqas.IQ, vec.new)

head(df.neqas.classified)

df.neqas.classified %>% arrange(desc(Median_Overall))
as.factor(vec.new)
class(df.neqas.classified$Median_Overall)

#### to do - use the classify at the start of the programme and start again




