# validate addresses using usps

validate_address_usps <- function(street, city, state, username) {
  state <-state.abb[grep(state, toupper(state.name))]
  rmwac <- function(x) x[ !names(x) %in% "ReturnText"]

  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }

  all_addresses <- data.frame()

  scheme_host_path <- paste0("http://production.shippingapis.com/",
                             "ShippingAPITest.dll?API=ZipCodeLookup&XML=",
                             "<ZipCodeLookupRequest USERID=\"")

  host_with_start_tag <- paste0(scheme_host_path, username, "\">")

  endtag <- "</ZipCodeLookupRequest>"

  n_ids <- length(street)

  remainder <- n_ids %% 5

  groups <- floor(n_ids / 5)

  for(j in seq(groups))  {

    address_ids <- c()

    for(i in 1:4) {
      id_number <- paste0("<Address ID=\"", i, "\">")

      address_part  <- paste0("<Address1></Address1><Address2>",
                              #street[(i + 1) + (j * 5) - 5], "</Address2><City>",
                              #city[(i + 1) + (j * 5) - 5], "</City><State>",
                              #state[(i + 1) + (j * 5) - 5], "</State></Address>")
                              street[(i + 1) ], "</Address2><City>",
                              city[(i + 1) ], "</City><State>",
                              state[(i + 1)], "</State></Address>")

      address_ids <- c(address_ids,  paste0(id_number, address_part))
    }

    body <- paste0(address_ids, collapse = "")

    URL <- paste0(host_with_start_tag, body, endtag, collapse = "")

    address_list <- xmlToList(xmlParse(URL))

    index.errors <- unlist(lapply(address_list, function(x) {names(x)[1]})) == "Error"

    address_list[index.errors] <- structure(list(Address = structure(list(Address2 = NA,
                                                                          City = NA, State = NA, Zip5 = NA, Zip4 = NA,
                                                                          .attrs = structure("0", .Names = "ID")), .Names = c("Address2",
                                                                                                                              "City", "State", "Zip5", "Zip4", ".attrs"))), .Names = "Address")

    usps_addresses <- do.call(rbind, lapply(address_list, rmwac))
    rownames(usps_addresses) <- NULL

    usps_addresses <- as.data.frame(usps_addresses)

    all_addresses <- rbind(all_addresses, usps_addresses)
  }

  for(l in (n_ids - remainder + 1) : n_ids) {

    id_number <- paste0("<Address ID=\"", 0, "\">")

    address_part  <- paste0("<Address1></Address1><Address2>",
                            street[l], "</Address2><City>",
                            city[l], "</City><State>",
                            state[l], "</State></Address>")
    body <- paste0(id_number, address_part, collapse = "")

    URL <- paste0(host_with_start_tag, body, endtag, collapse = "")

    address_list <- xmlToList(xmlParse(URL))

    index.errors <- unlist(lapply(address_list, function(x) {names(x)[1]})) == "Error"

    address_list[index.errors] <- structure(list(Address = structure(list(Address2 = NA,
                                                                          City = NA, State = NA, Zip5 = NA, Zip4 = NA,
                                                                          .attrs = structure("0", .Names = "ID")),
                                                                     .Names = c("Address2",
                                                                                "City", "State", "Zip5", "Zip4", ".attrs"))), .Names = "Address")


    usps_addresses <- do.call(rbind, lapply(address_list, rmwac))

    rownames(usps_addresses) <- NULL

    usps_addresses <- as.data.frame(usps_addresses)

    all_addresses <- rbind(all_addresses, usps_addresses)

  }

  return(all_addresses)

}


#If you end up doing this for a long tabular format of data and go through each row, there are null rows that return as results
# to remove the NA rows, try something like:
#for(i in 1:nrow(df)){
#  street1 <- df$`Location Address 1 [Public School] 2015-16`[i]
#  city <- df$`Location City [Public School] 2015-16`[i]
#  state <- df$State[i]
#  temp <- validate_address_usps(street1, city, state, username)
#  row.has.na <- apply(temp, 1, function(x){any(is.na(x))})    #this line and the next line remove all the NA entries are produced when the API call returns
#  temp <- temp[!row.has.na,]
#  if(length(temp$Zip5)==1){
#    df$Zipcode[i] <- as.numeric(temp$Zip5[[1]])
#  }else{
#    df$Zipcode[i] <- NA
#  }
