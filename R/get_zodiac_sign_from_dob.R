get_zodiac_sign_from_dob <- function(dob){
  return(tolower(DescTools::Zodiac(dob)))
}
