context("Entry QA/QC")

test_that("data_files directory contains QAQC directory",
          expect_true(assertthat::assert_that(dir.exists(file.path(dataset_directory,"QAQC"))))
)

test_that("data_files directory contains database directory",
          expect_true(assertthat::assert_that(dir.exists(file.path(dataset_directory,"database"))))
)

test_that("all .xlsx files can be read",
          lapply(seq_along(data_files), function(x) {
            expect_silent(openxlsx::read.xlsx(data_files[x]))
          })
)

test_that("entry matches template file", {
  lapply(seq_along(data_files), function(x) {
    expect_true(all(getSheetNames(data_files[x]) %in% names(template)) | all(names(template) %in% getSheetNames(data_files[x])))
  })
})

test_that("general QAQC", {
  for(f in seq_along(data_files)) {
    #start error count at 0
    error<-0
    data<-lapply(getSheetNames(data_files[f])[1:8], function(s) read.xlsx(data_files[f] , sheet=s))
    names(data)<-getSheetNames(data_files[f])[1:8]
    if(!(all(lapply(data, function(x) x[1,1])=="Entry/Dataset Name") & all(lapply(data, function(x) x[2,1])=="Author_year"))){
      cat("\n\tWARNING:  Description rows in data file not detected. The first two rows of your data file should be the description rows as found in the template file.");error<-error+1
    }

    # trim description/empty rows
    data<-lapply(data, function(x) x<-x[-1:-2,])
    for (i in seq_along(data)){
      tab<-data[[i]]
      for (j in seq_len(ncol(tab))){
        tab[,j][grep("^[ ]+$", tab[,j])]<-NA
      }
      data[[i]]<-tab
      data[[i]]<-data[[i]][rowSums(is.na(data[[i]])) != ncol(data[[i]]),]
    }
    data<-lapply(data, function(x) lapply(x, as.character))
    data<-lapply(data, function(x) lapply(x, utils::type.convert))
    data<-lapply(data, as.data.frame)

    ##### check for extra or misnamed columns ####
    for (t in seq_along(names(data))) {
      tab<-names(data)[t]
      data_colnames<-colnames(data[[tab]])
      template_colnames<-colnames(template[[tab]])

      #compare column names in data to template column names
      notintemplate<-setdiff(data_colnames, template_colnames)
      if (length(notintemplate>0)) {
        error<-error+1
      }
    }

    ##### check for missing values in required columns ####
    for (t in seq_along(names(data))){
      tab<-names(data)[t]
      required_colnames<-template_info[[tab]]$Column_Name[template_info[[tab]]$Required=="Yes"]

      missing_values<-sapply(required_colnames, function(c) NA %in% data[[tab]][[c]])
      T %in% unlist(missing_values)
      which_missing_values<-unlist(sapply(required_colnames[missing_values], function(c) unlist(which(is.na(data[[tab]][[c]])))))

      if (T %in% unlist(missing_values)) {
        error<-error+1
      }
    }

    ##### check levels #####
    # check site tab #
    mismatch <- c() #Entry name
    for (t in seq_along(data$site$entry_name)){
      item_name <- as.character(data$site$entry_name)[t]
      if (!(item_name %in% data$metadata$entry_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    duplicates <- data$site %>% select(.data$entry_name, .data$site_lat, .data$site_long) %>% duplicated() %>% which()
    if(length(duplicates)>0){
      error <- error+1
    }
    duplicates <- data$site %>% select(.data$entry_name, .data$site_name) %>% duplicated() %>% which()
    if(length(duplicates)>0){
      error <- error+1
    }

    # check profile tab #
    mismatch <- c() #Entry name
    for (t in seq_along(data$profile$entry_name)){
      item_name <- as.character(data$profile$entry_name)[t]
      if (!(item_name %in% data$metadata$entry_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Site name
    for (t in seq_along(data$profile$site_name)){
      item_name <- as.character(data$profile$site_name)[t]
      if (!(item_name %in% data$site$site_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch.rows <- anti_join(as.data.frame(lapply(data$profile, as.character), stringsAsFactors = FALSE), as.data.frame(lapply(data$site, as.character), stringsAsFactors = FALSE), by=c("entry_name","site_name"))
    if(dim(mismatch.rows)[1]>0){
      row.ind <- match(data.frame(t(mismatch.rows[,c("entry_name","site_name")])),
                       data.frame(t(data$profile[,c("entry_name","site_name")])))
      error <- error+1
    }

    duplicates <- data$profile %>% select(.data$entry_name, .data$site_name, .data$pro_name) %>% duplicated() %>% which()
    if(length(duplicates)>0){
      error <- error+1
    }


    # check flux tab #
    if (length(data$flux$entry_name)>0){
    mismatch <- c() #Entry name
    for (t in seq_along(data$flux$entry_name)){
      item_name <- as.character(data$flux$entry_name)[t]
      if (!(item_name %in% data$metadata$entry_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Site name
    for (t in seq_along(data$flux$site_name)){
      item_name <- as.character(data$flux$site_name)[t]
      if (!(item_name %in% data$site$site_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Profile name
    for (t in seq_along(data$flux$pro_name)){
      item_name <- as.character(data$flux$pro_name)[t]
      if (!(item_name %in% data$profile$pro_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }


    mismatch.rows <- anti_join(as.data.frame(lapply(data$flux, as.character), stringsAsFactors = FALSE), as.data.frame(lapply(data$site, as.character), stringsAsFactors = FALSE), by=c("entry_name","site_name"))
    if(dim(mismatch.rows)[1]>0){
      row.ind <- match(data.frame(t(mismatch.rows[,c("entry_name","site_name")])),
                       data.frame(t(data$flux[,c("entry_name","site_name")])))
      error <- error+1
    }


    if("flx_name" %in% colnames(data$flux)) {
        duplicates <- data$flux %>% select("entry_name","site_name","pro_name","flx_name") %>% duplicated() %>% which()
        if(length(duplicates)>0){
        error <- error+1
        }
      } else {
          duplicates <- data$flux %>% select("entry_name","site_name","pro_name") %>% duplicated() %>% which()
          if(length(duplicates)>0){
            error <- error+1
        }
      }
    }

    # check layer tab #
    if (length(data$layer$entry_name)>0){
    mismatch <- c() #Entry name
    for (t in seq_along(data$layer$entry_name)){
      item_name <- as.character(data$layer$entry_name)[t]
      if (!(item_name %in% data$metadata$entry_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Site name
    for (t in seq_along(data$layer$site_name)){
      item_name <- as.character(data$layer$site_name)[t]
      if (!(item_name %in% data$site$site_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Profile name
    for (t in seq_along(data$layer$pro_name)){
      item_name <- as.character(data$layer$pro_name)[t]
      if (!(item_name %in% data$profile$pro_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch.rows <- anti_join(as.data.frame(lapply(data$layer, as.character), stringsAsFactors = FALSE), as.data.frame(lapply(data$profile, as.character), stringsAsFactors = FALSE), by=c("entry_name","site_name","pro_name"))
    if(dim(mismatch.rows)[1]>0){
      row.ind <- match(data.frame(t(mismatch.rows[,c("entry_name","site_name","pro_name")])),
                       data.frame(t(data$layer[,c("entry_name","site_name","pro_name")])))
      error <- error+1
    }

    duplicates <- data$layer %>% select(ends_with("name")) %>% duplicated() %>% which()
    if(length(duplicates)>0){
      error <- error+1
    }

    lyr_depth_err <- which(data$layer$lyr_bot < data$layer$lyr_top)
    if(length(lyr_depth_err > 0)){
      error <- error+1
    }
  }

    # check interstitial tab #
    if (length(data$interstitial$entry_name)>0){
    mismatch <- c() #Entry name
    for (t in seq_along(data$interstitial$entry_name)){
      item_name <- as.character(data$interstitial$entry_name)[t]
      if (!(item_name %in% data$metadata$entry_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Site name
    for (t in seq_along(data$interstitial$site_name)){
      item_name <- as.character(data$interstitial$site_name)[t]
      if (!(item_name %in% data$site$site_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Profile name
    for (t in seq_along(data$interstitial$pro_name)){
      item_name <- as.character(data$interstitial$pro_name)[t]
      if (!(item_name %in% data$profile$pro_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch.rows <- anti_join(as.data.frame(lapply(data$interstitial, as.character), stringsAsFactors = FALSE), as.data.frame(lapply(data$profile, as.character), stringsAsFactors = FALSE), by=c("entry_name","site_name","pro_name"))
    if(dim(mismatch.rows)[1]>0){
      row.ind <- match(data.frame(t(mismatch.rows[,c("entry_name","site_name","pro_name")])),
                       data.frame(t(data$interstitial[,c("entry_name","site_name","pro_name")])))
      error <- error+1
    }

    duplicates <- data$interstitial %>% select(ends_with("name")) %>% duplicated() %>% which()
    if(length(duplicates)>0){
      error <- error+1
    }
    }

    # check fraction tab #
    if (length(data$fraction$entry_name)>0){

    mismatch <- c() #Entry name
    for (t in seq_along(data$fraction$entry_name)){
      item_name <- as.character(data$fraction$entry_name)[t]
      if (!(item_name %in% data$metadata$entry_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Site name
    for (t in seq_along(data$fraction$site_name)){
      item_name <- as.character(data$fraction$site_name)[t]
      if (!(item_name %in% data$site$site_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Profile name
    for (t in seq_along(data$fraction$pro_name)){
      item_name <- as.character(data$fraction$pro_name)[t]
      if (!(item_name %in% data$profile$pro_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Layer name
    for (t in seq_along(data$fraction$lyr_name)){
      item_name <- as.character(data$fraction$lyr_name)[t]
      if (!(item_name %in% data$layer$lyr_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch.rows <- anti_join(as.data.frame(lapply(data$fraction, as.character), stringsAsFactors = FALSE), as.data.frame(lapply(data$layer, as.character), stringsAsFactors = FALSE), by=c("entry_name","site_name","pro_name","lyr_name"))
    if(dim(mismatch.rows)[1]>0){
      row.ind <- match(data.frame(t(mismatch.rows[,c("entry_name","site_name","pro_name","lyr_name")])),
                       data.frame(t(data$fraction[,c("entry_name","site_name","pro_name","lyr_name")])))
      error <- error+1
    }

    duplicates <- data$fraction %>% select(ends_with("name")) %>% duplicated() %>% which()
    if(length(duplicates)>0){
      error <- error+1
    }
    }

    # check incubation tab #
    if (length(data$incubation$entry_name)>0){
    mismatch <- c() #Entry name
    for (t in seq_along(data$incubation$entry_name)){
      item_name <- as.character(data$incubation$entry_name)[t]
      if (!(item_name %in% data$metadata$entry_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Site name
    for (t in seq_along(data$incubation$site_name)){
      item_name <- as.character(data$incubation$site_name)[t]
      if (!(item_name %in% data$site$site_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Profile name
    for (t in seq_along(data$incubation$pro_name)){
      item_name <- as.character(data$incubation$pro_name)[t]
      if (!(item_name %in% data$profile$pro_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch <- c() #Layer name
    for (t in seq_along(data$incubation$lyr_name)){
      item_name <- as.character(data$incubation$lyr_name)[t]
      if (!(item_name %in% data$layer$lyr_name)){
        mismatch <- c(mismatch, t+3)
      }
    }
    if (length(mismatch) > 0){
      error <- error+1
    }

    mismatch.rows <- anti_join(as.data.frame(lapply(data$incubation, as.character), stringsAsFactors = FALSE), as.data.frame(lapply(data$layer, as.character), stringsAsFactors = FALSE), by=c("entry_name","site_name","pro_name","lyr_name"))
    if(dim(mismatch.rows)[1]>0){
      row.ind <- match(data.frame(t(mismatch.rows[,c("entry_name","site_name","pro_name","lyr_name")])),
                       data.frame(t(data$incubation[,c("entry_name","site_name","pro_name","lyr_name")])))
      error <- error+1
    }

    duplicates <- data$incubation %>% select(ends_with("name")) %>% duplicated() %>% which()
    if(length(duplicates)>0){
      error <- error+1
    }
  }

  ##### check numeric values #####
  which.nonnum <- function(x) {
    badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
    which(badNum & !is.na(x))
  }
  for (t in seq_along(names(data))){
    tab<-names(data)[t]
    tab_info<-template_info[[tab]]

    #check for non-numeric values where required
    numeric_columns<-tab_info$Column_Name[tab_info$Variable_class=="numeric"]
    if(length(numeric_columns)<1) next
    if(tab %in% emptytabs) next
    for (c in seq_along(numeric_columns)){
      column<-numeric_columns[c]
      if(!column %in% colnames(data[[tab]])) next
      nonnum<-!is.numeric(data[[tab]][,column]) & !is.logical(data[[tab]][,column])
      if(nonnum) {
        error<-error+1
      } else {
        max<-as.numeric(tab_info$Max[tab_info$Column_Name == column])
        min<-as.numeric(tab_info$Min[tab_info$Column_Name == column])
        toobig<-data[[tab]][,column]>max
        toosmall<-data[[tab]][,column]<min
        if(sum(toobig, na.rm=TRUE)>0) {
          error<-error+1
        }

        if(sum(toosmall, na.rm=TRUE)>0) {
          error<-error+1
        }

      }

    }

  }

  ##### check controlled vocab -----------------------------------------------
  for (t in 2:length(names(data))){
    tab<-names(data)[t]
    tab_info<-template_info[[tab]]

    #check for non-numeric values where required
    controlled_vocab_columns<-tab_info$Column_Name[tab_info$Variable_class=="character" & !is.na(tab_info$Vocab)]
    for (c in seq_along(controlled_vocab_columns)){
      column<-controlled_vocab_columns[c]
      if(!column %in% colnames(data[[tab]])) next
      controlled_vocab<-tab_info$Vocab[tab_info$Column_Name == column]
      controlled_vocab<-unlist(strsplit(controlled_vocab, ","))
      controlled_vocab<-sapply(controlled_vocab, trimws)
      if(controlled_vocab[1]=="must match across levels") next
      vocab_check<-sapply(data[[tab]][,column], function(x) x %in% c(controlled_vocab, NA))
      if(F %in% vocab_check){
        error<-error+1
      }

    }

  }

  expect_equal(error, 0)
  }
})
