% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Deepcelltype.R
\name{Deepcelltype}
\alias{Deepcelltype}
\title{Single-cell annotation using various large language models.}
\usage{
Deepcelltype(
  input,
  tissuename = NULL,
  anno_model = "deepseek-ai/DeepSeek-R1",
  API_KEY = NULL,
  Note = NULL
)
}
\arguments{
\item{input}{a list of genes.}

\item{tissuename}{input of tissue name.}

\item{anno_model}{You can use many models, including DeepSeek. For details, check https://siliconflow.cn/zh-cn/. The default model is deepseek-ai/DeepSeek-R1}

\item{API_KEY}{The Deepseek key. The default is NULL, which will resulting outputing the prompt itself. If an actual key is provided, then the output will be the celltype annotations from the GPT model specified by the user.}

\item{Note}{Default is 'NULL'.You can make some detailed customizations based on our foundation, such as: 'Please provide more specific subtype annotations for the cells, not just the broad categories}
}
\description{
Through various large language models, we can annotate single cells in a Seurat pipeline. We can use a custom gene list.For the available models, you can refer to https://siliconflow.cn/zh-cn/.
}
\details{
The Ultimate Large Language Model for Cell Type Annotation
}
\author{
Linfeng HangYang
}
