encoding <- function(text)
{
  text <- as.character(text)
  return(stri_enc_toutf8(stri_unescape_unicode(stri_replace_all_regex(text,"<U\\+([[:alnum:]]+)>", "\\\\u$1"))))
}
