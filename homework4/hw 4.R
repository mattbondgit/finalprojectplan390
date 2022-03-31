library(tidyverse)
library(rvest)
library(polite)
library(purrr)
library(ggplot2)
session = bow("https://www.ncleg.gov/Members/MemberList/S")
res = scrape(session, list("s"=""))
elements = html_elements(res, ".member-col")
senate = map_dfr(elements, function (elements) {
  names = html_elements(elements, ".pr-0 p:nth-child(1) a") %>% html_text2()
  district = html_elements(elements, ".text-nowrap+ p a , p:nth-child(2) a") %>% html_text2()
  senate_url = html_element(elements, "p:nth-child(1) a") %>% html_attr("href")
  return(list(
    "names"=names,
    "district"=district,
    "senate_url"=senate_url
   ))
  })
get_term = function(url) {
  session <<- nod(session, url)
  res = scrape(session)
  term = html_elements(res, ".col-xl-6:nth-child(2) p") %>%
    first() %>%
    html_text2()
  return(term)
}
get_term(url = "/Members/Biography/S/396")

with_term = rowwise(senate, everything()) %>%
  mutate(term=get_term(senate_url))

get_party = function(url) {
  session <<- nod(session, url)
  res = scrape(session)
  party = html_elements(res, "h6.text-nowrap") %>%
    first() %>%
    html_text2()
  return(party)
}
get_party(url ="/Members/Biography/S/396" )

with_party = rowwise(with_term, everything()) %>%
  mutate(party=get_party(senate_url))

as.numeric(str_extract(with_party$term, "^[0-9]+"))

with_party = mutate(with_party, term=as.numeric(str_extract(term, "^[0-9]+")))
# Converted Term to a single number
as.numeric(str_extract(with_party$district, "[0-9]+$"))

with_party = mutate(with_party, district=as.numeric(str_extract(district, "[0-9]+$")))
# Converted district to a number
with_party = mutate(with_party, party=str_extract(party, "[:alpha:]?"))
# Converted party to a number
with_party = mutate(with_party, term=)

write.csv(with_party, "ncsenate.csv", na = "1")
# Wrote the data into a csv while also converting the NA term for Syndney Batch as 1
data = read_csv("ncsenate.csv")
group_by(data, party) %>%
  summarize(mean_term=mean(term))
# Republicans on average have served for a slightly longer time than Democrats

library(sf)
map = read_sf("Senate Consensus Nonpartisan Map v3.shp")
map_new = rename(map, district = DISTRICT)
map_newer = mutate(map_new, district=as.numeric(district))

nc_districts = left_join(map_newer, data, by="district")
nc_districts = st_transform(nc_districts, 32119)

ggplot() +
  geom_sf(data=nc_districts, color="red", aes(fill=party))
  
ggplot() +
  geom_sf(data=nc_districts, color="red", aes(fill=term))



