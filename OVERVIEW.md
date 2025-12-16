# NA

\<% \## Reuse the future vignette md \<-
R.rsp::rstring(file=“vignettes/future.batchtools.md.rsp”,
postprocess=FALSE)

## Drop the header, i.e. anything before the first “H2” header

md \<- unlist(strsplit(md, split=“”, fixed=TRUE)) row \<- grep(“^##”,
md)\[1\] if (!is.na(row)) md \<- md\[-seq_len(row-1)\]

## Drop the footer, i.e. anything after the first horizontal line

row \<- grep(“^—”, md)\[1\] if (!is.na(row)) md \<- md\[seq_len(row-1)\]

## Output

cat(md, sep=“”) %\>
