
library(reticulate)

myenvs=conda_list()
myenvs
#envname=myenvs$name[2]
#use_condaenv(envname, required = TRUE)


library(huggingfaceR)
#hf_python_depends()

distilBERT <- hf_load_pipeline(
  model_id = "distilbert-base-uncased-finetuned-sst-2-english", 
  task = "text-classification"
)
