# Deepcelltype

## Overview
Deepcelltype is a tool designed for single-cell annotation using Deepseek in a Seurat pipeline. Users can annotate single cells based on custom gene lists. The tool is highly adaptable, and you also can choose from several models for accurate cell type predictions.

For more information about the available models, visit [SiliconFlow](#).

## Features
- **Gene List Input**: Annotate single cells based on a custom gene list.
- **Tissue Name Input**: Include the tissue name for more context during annotation.
- **Multiple Annotation Models**: Supports various models, including DeepSeek, for precise cell type classification.
- **Flexible Customization**: Customize annotations by providing specific notes and refining the output from the language models.

## Installation
To use this tool, you’ll need to install the necessary dependencies. The main package dplyr is required for data manipulation.
```
install.packages("dplyr")
```

### Installation Methods
#### 1. From GitHub (Recommended)
If the package is hosted on GitHub:

```
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install from GitHub repository
devtools::install_github("fengqlin/Deepcelltype")  # Replace with actual repository path
```

## Refrence
NULL
