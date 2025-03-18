#' The Ultimate Large Language Model for Cell Type Annotation
#'
#' @title Single-cell annotation using various large language models.
#'
#' @description Through various large language models, we can annotate single cells in a Seurat pipeline. We can use a custom gene list.For the available models, you can refer to https://siliconflow.cn/zh-cn/.
#'
#' @param input a list of genes.
#' @param tissuename input of tissue name.
#' @param anno_model You can use many models, including DeepSeek. For details, check https://siliconflow.cn/zh-cn/. The default model is deepseek-ai/DeepSeek-R1
#' @param API_KEY The Deepseek key. The default is NULL, which will resulting outputing the prompt itself. If an actual key is provided, then the output will be the celltype annotations from the GPT model specified by the user.
#' @param Note Default is 'NULL'.You can make some detailed customizations based on our foundation, such as: 'Please provide more specific subtype annotations for the cells, not just the broad categories
#' @import dplyr
#' @export
#' @author Linfeng HangYang
#' @references NULL

Deepcelltype <- function(input, tissuename = NULL,
                          anno_model = "deepseek-ai/DeepSeek-R1",
                          API_KEY = NULL,Note=NULL) {

  # Check if the DEEPSEEK API key is provided
  if (is.null(API_KEY) || API_KEY == "") {
    message("Warning: DEEPSEEK API key is missing.")
    api_flag <- FALSE
  } else {
    api_flag <- TRUE
  }


  # concatenate each element into a comma-separated string
  input <- sapply(input, paste, collapse = ',')

  # If no API key, return the constructed message
  if (!api_flag) {
    message_text <- paste(
      "You are an expert in scRNA cell annotation. You are now annotating the cells from", tissuename, "samples.\n",
      "Identify the cell types or mixtures of cell types using the following markers for each row. Only provide the cell type name.\n",
      paste(names(input), ":", unlist(input), collapse = "\n")
    )
    return(message_text)
  }

  # If API key is available, make the API request
  message("Using DEEPSEEK API for cell annotation...")

  # Split the input data into chunks to avoid API timeouts
  num_chunks <- ceiling(length(input) / 30)
  chunk_ids <- rep(1:num_chunks, each = ceiling(length(input) / num_chunks), length.out = length(input))

  # Loop through the chunks using sapply and collect results
  results <- sapply(1:num_chunks, function(chunk_index) {
    chunk_data <- which(chunk_ids == chunk_index)


    # Construct the message content
    message_text <- paste(
      Note,
      "You are an expert in scRNA cell annotation. You are now annotating the cells from", tissuename, "samples.\n",
      "The following annotations are from your multiple rounds of annotation. Provide the most likely cell type and a confidence score (range 0-1). Only return the cell type and confidence score.\n",
      paste(names(input)[chunk_data], ":", input[chunk_data], collapse = "\n")
    )

    # Call RchatSF function to send the request and get the response
    response <- RchatSF(
      API_KEY,
      model = anno_model,
      max_tokens = 8190,
      temperature = 0.6,
      top_k = 50,
      top_p = 0.7,
      frequency_penalty = 0,
      auto_input = message_text
    )

    # Process and clean the returned result
    processed_result <- strsplit(response, '\n')[[1]]
    processed_result <- gsub("^\\d+\\.\\s*", "", processed_result)
    processed_result <- gsub("^\\d+\\:\\s*", "", processed_result)
    processed_result <- trimws(processed_result)

    # If the number of results matches the expected length, stop retrying
    if (length(processed_result) == length(chunk_data)) {
      retry_flag <- FALSE
    }


    # Assign names to the results corresponding to the input names
    names(processed_result) <- names(input)[chunk_data]
    processed_result
  }, simplify = FALSE)

  # Combine all chunk results and return
  final_result <- unlist(results)

  # Return the final result with the trailing commas removed
  return(gsub(",$", "", final_result))
}
RchatSF <- function(api_key, model = "deepseek-ai/DeepSeek-R1", max_tokens = 8190, temperature = 0.6, top_k = 50, top_p = 0.7, frequency_penalty = 0, auto_input = NULL) {
  .chat_completion <- function(messages, api_key, model, max_tokens, temperature, top_k, top_p, frequency_penalty) {
    API_URL <- "https://api.siliconflow.cn/v1/chat/completions"

    headers <- httr::add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    )

    payload <- list(
      model = model,
      messages = messages,
      temperature = temperature,
      max_tokens = max_tokens,
      top_k = top_k,
      top_p = top_p,
      frequency_penalty = frequency_penalty,
      response_format = list(type = "text")
    )

    tryCatch({
      response <- httr::POST(
        API_URL,
        headers,
        body = jsonlite::toJSON(payload, auto_unbox = TRUE),
        encode = "json",
        httr::timeout(6000)
      )

      if (httr::status_code(response) != 200) {
        if (httr::status_code(response) %in% c(504, 503)) {
          stop(paste("API request failed, server is busy"))
          return(NULL)
        } else {
          stop(paste("API request failed, status code:", httr::status_code(response)))
          return(NULL)
        }
      }

      content <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      content$choices$message$content
    }, error = function(e) {
      message(paste("Error:", e$message))
      return(NULL)
    })
  }

  cat("\033[32mConversation started, type 'exit' to end the conversation\033[0m\n")
  messages <- list()
  AI = TRUE
  while (AI) {

    # If there is an automatic input, use it
    user_input <- auto_input
    cat("\033[36m \033[0m", user_input, "\n")

    messages <- append(messages, list(list(role = "user", content = user_input)))
    cat("\033[33mThinking...\033[0m\n")

    response <- .chat_completion(messages, api_key, model, max_tokens, temperature, top_k, top_p, frequency_penalty)

    if (is.null(response)) {
      cat("\033[31mRequest failed, please check the network or API key\033[0m\n")
      messages <- messages[-length(messages)] # Remove invalid user input
      next
    }

    messages <- append(messages, list(list(role = "assistant", content = response)))
    cat("\n\033[35mAnswer:\033[0m\n", response, "\n", strrep("-", 50), "\n\n")
    return(response)
    break
  }
  AI = FALSE
}
