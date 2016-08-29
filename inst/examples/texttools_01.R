# digorig::do.init()
library(texttools)

# "removable_chars";""''`´¨{}+^*/\.;:-_¿?¡!"

texttools::text.load_data()

nchar(listReplacements[["removable_chars"]])

message(listReplacements["removable_chars"])

order_desc = dfRequests$order_desc
order_desc = texttools::text.remove_tabs(order_desc)
order_desc = texttools::text.remove_newlines(order_desc)
order_desc = texttools::text.remove_extra_characters_readable(order_desc)
order_desc = chartr(removable_chars, paste0(rep(" ", nchar(removable_chars)), collapse=""), order_desc)
order_desc = texttools::text.remove_extra_spaces(order_desc)
order_desc = texttools::text.trim(order_desc)
order_desc = texttools::text.toASCII(order_desc)
