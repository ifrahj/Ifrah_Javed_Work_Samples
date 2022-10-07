# Functions to clean raw data

parse_json_col <- function(col_, col_name_, field_='name', thresh_=1) {
  # fun to extrapolate dummy cols from json col; limited to freq. thresh.
  require(stringr)
  require(purrr)
  require(dplyr)
  
  # define json field pattern
  if (field_ != 'id') {
    pattern <- paste0('(?<="', 
                      field_, 
                      '": ").*?(?="[\\}|", "])')
  } else {
    pattern <- paste0('(?<="', 
                      field_, 
                      '": ).*?(?=[\\}|, "])')
  }
  # get matrix of terms
  term_mx <- matrix(stringr::str_match_all(col_, 
                                           pattern))
  # convert to frequency table
  freq_counts <- table(gsub("[^[:alnum:]]", "", unlist(term_mx)))
  # get names over threshold
  over_thresh <- names(freq_counts[freq_counts >= thresh_])
  # get df over thresh
  df_ <- as.data.frame(term_mx) %>% 
    mutate(V1 = purrr::map(V1, 
                           ~gsub("[^[:alnum:]]", 
                                 "", 
                                 .x)[gsub("[^[:alnum:]]", 
                                                              "", 
                                                              .x) 
                                                         %in% over_thresh]))
  # pre-populate all cols with FALSE
  df_[over_thresh] <- FALSE
  # replace with true where present
  df_ <- df_ %>% rowwise() %>% 
    mutate(across(all_of(over_thresh), ~cur_column() %in% V1)) %>% 
    select(-V1)
  # add field prefix
  colnames(df_) <- paste(col_name_, colnames(df_), sep = '.')
  return(df_)
}

apply_json_parse <- function(df_, col_lst=c('genres', 
                                            'production_countries',
                                            'production_companies',
                                            'spoken_languages'),
                             field_lst=c('name', 
                                         'iso_3166_1', 
                                         'id', 
                                         'iso_639_1')) {
  require(dplyr)
  # loop through col lst
  for (i in sequence(length(col_lst))) {
    # apply json function
    df_ <- cbind(df_ %>% select(-all_of(col_lst[i])),
                 parse_json_col(df_[[col_lst[i]]], col_lst[i],
                                field_ = field_lst[i]))
  }
  return(df_)
}

get_name_id_pairs <- function(col_,
                              field_lst = c('id', 'name'),
                              thresh_=1) {
  require(dplyr)
  
  idx <- 1
  for (field_ in field_lst) {
    if (field_ != 'id') {
      pattern <- paste0('(?<="', 
                        field_, 
                        '": ").*?(?="[\\}|" , "])')
    } else {
      pattern <- paste0('(?<="', 
                        field_, 
                        '": ).*?(?=[\\}| , "])')
    }
    
    new_col <- as.data.frame(unlist(matrix(stringr::str_match_all(col_,
                                                                  pattern))))
    colnames(new_col) <- field_
    if (idx != 1) {
      value_df <- cbind(value_df,
                        new_col)
    } else {
      value_df <- new_col
    }
    idx = idx + 1
  }
  return(value_df %>% group_by(across(all_of(field_lst))) %>% count() %>% 
           filter(n >= thresh_) %>%
           arrange(desc(n)))
}

adj_mil_units <- function(df) {
  df <- df %>% mutate(budget = ifelse((budget < 1e3) & (id != 1435), 
                                      budget*1e6, budget),
                      revenue = ifelse(revenue < 1e3, revenue*1e6, revenue))
}

clean_data <- function(df_,
                       country_map_path ='./data/mappings/country_classifications.csv',
                       data_impact_path = './data/processed/data_impact.Rdata') {
  require(dplyr)
  require(lubridate)
  
  base_size <- nrow(df_)
  
  # convert factor columns to factors
  factor_cols <- c('original_language')
  
  return_df <- df_ %>% mutate(across(all_of(factor_cols), factor)) %>%
    # convert release date to date
    mutate(release_date = as.Date(release_date, '%Y-%m-%d')) %>%
    # filter non released movies
    filter(status == 'Released')
  
  released_size <- nrow(return_df)
  
  return_df <- return_df %>%
    # filter to revenue > 0
    filter(revenue > 0)
  
  rev_size <- nrow(return_df)
  
  corrections = as.data.frame(cbind(c(28932, 217708, 78383, 22649, 108346, 3082, 51941),
                                    c(20300000, 239969, 80231, 4500000, 12044, 1800000, 467272),
                                    c(8000000, 10000000, 10000000, 799529, 3250000, 1500000, 250000)))
  colnames(corrections) <- c('id', 'revenue', 'budget')
  
  for(id in corrections$id){
    return_df[return_df$id == id, 
              c('revenue', 'budget')] <- corrections[corrections$id == id,
                                                     c('revenue', 'budget')]
  }
  return_df <- adj_mil_units(return_df)
  
  return_df <- return_df %>%
    # filter to budget > 1e6
    filter(budget > 1e6)
  
  bud_size <- nrow(return_df)
    
  return_df <- return_df %>%
    # apply json functions
    apply_json_parse() %>%
    # replace cn language with zh correction
    mutate(`spoken_languages.zh` = as.logical(pmax(`spoken_languages.zh`,
                                                   `spoken_languages.cn`))) %>%
    select(-`spoken_languages.cn`)
  
  return_df <- return_df %>%
    mutate(genres_n = rowSums(select(., starts_with('genres.')))) %>%
    mutate(spoken_languages_n = rowSums(select(., starts_with('spoken_languages.')))) %>%
    mutate(production_companies_n = rowSums(select(., starts_with('production_companies.')))) %>%
    mutate(production_countries_n = rowSums(select(., starts_with('production_countries.')))) %>%
    mutate(english_original_language = (original_language == 'en'))
  
  country_map <- read.csv(country_map_path)
  colnames(country_map) <- c('id', 'region')
  
  # map region to get country map
  for (region in unique(country_map$region)) {
    return_df <- return_df %>% 
      mutate(!!sym(paste0('production_regions.', region)) := 
               as.logical(do.call(pmax, subset(., select = 
                                                 all_of(paste0('production_countries.',
                                                               unlist(country_map[country_map$region==region,
                                                                                  'id'])))))))
  }
  
  # write data_impact to processed
  data_impact <- as.data.frame(cbind(c('Raw', 'Released Only', 'Revenue > $0', 'Budget > $1M'),
                               c(base_size, released_size, rev_size, bud_size)))
  
  colnames(data_impact) <- c('Filter', 'n')
  
  data_impact <- data_impact %>%
    mutate(n = as.integer(n),
           impact = scales::percent_format(accuracy = 0.1)(n/lag(n)-1))
  
  # save impact
  save(data_impact, file = data_impact_path)
  
  return(return_df)
}

# process data
process_data <- function(raw_path = './data/raw/tmdb_5000_movies.csv',
                         company_map_path = './data/mappings/company_id_map.RData',
                         corrections_path = './data/processed/rev_budget_corrections.RData',
                         final_out_full = './data/processed/full_processed.RData',
                         final_out_exp = './data/processed/exploratory_set.RData',
                         final_out_trn = './data/processed/train_set.RData') {
  require(dplyr)
  require(lubridate)
  
  # set seed
  set.seed(42)
  
  # read data
  data <- read.csv(raw_path)
  
  # clean_data
  data_clean <- clean_data(data)
  
  # write clean full set
  save(data_clean, file = final_out_full)
  
  # production companies match list
  company_map <- get_name_id_pairs(data[['production_companies']],
                                   field_lst = c('id', 'name'),
                                   thresh_=1)
  
  save(company_map, file = company_map_path)
  
  # summarize rev/budget corrections
  corrections_summary = data_clean[, c('id', 'title', 'release_date', 
                                       'budget', 'revenue')] %>% 
    inner_join(data[, c('id', 'revenue', 'budget')], 
               by='id', suffix = c('_corrected','_orig')) %>%
    filter((budget_orig != budget_corrected) | (revenue_orig != revenue_corrected))
  
  save(corrections_summary, file = corrections_path)
  
  # split into train/test set
  data_size <- nrow(data_clean)
  sample <- sample.int(n = data_size, size = floor(.4*data_size), replace = F)
  data_exp <- data_clean[sample, ]
  data_train  <- data_clean[-sample, ]
  
  save(data_exp, file = final_out_exp)
  save(data_train, file = final_out_trn)
}
