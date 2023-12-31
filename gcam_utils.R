library(dplyr)  # needed for pipelines

read_land_inputs_xml2 <- function(folder, protected){
  
  land1 <- xml2::read_xml(paste0(folder,"/land_input_1.xml"))
  land1_root <- xml2::xml_root(land1)

  land2 <- xml2::read_xml(paste0(folder,"/land_input_2.xml"))
  land2_root <- xml2::xml_root(land2)

  land3 <- xml2::read_xml(paste0(folder,"/land_input_3_IRR.xml"))
  land3_root <- xml2::xml_root(land3)

  land4 <- xml2::read_xml(paste0(folder,"/land_input_4_IRR_MGMT.xml"))
  land4_root <- xml2::xml_root(land4)

  land5 <- xml2::read_xml(paste0(folder,"/land_input_5_IRR_MGMT.xml"))
  land5_root <- xml2::xml_root(land5)

  if(protected == TRUE){
  protland2 <- xml2::read_xml(paste0(folder,"/protected_land_input_2.xml"))
  protland2_root <- xml2::xml_root(protland2)
  
  protland3 <- xml2::read_xml(paste0(folder,"/protected_land_input_3.xml"))
  protland3_root <- xml2::xml_root(protland3)
  
  land_roots <- list(land1_root,
                     land2_root,
                     land3_root,
                     land4_root,
                     land5_root,
                     protland2_root,
                     protland3_root)}
  else{
    land_roots <- list(land1_root,
                       land2_root,
                       land3_root,
                       land4_root,
                       land5_root)}
  return(land_roots)}


process_xml_inputs <- function(land_roots, gcam_land_alloc, nleaves=0, nrows=0){

  # make dataframe
  # Units | scenario | region | landleaf | year | value | variable

  table_cols <- c("region","landleaf","year","land_alloc")

  all_data <- data.frame(region=character(),
                         landleaf=character(),year=integer(),
                         land_alloc=double())

  colnames(all_data) <- table_cols


  # process leaves in each land root at a time
  count <- 0
  for (i in 1:5){
    data <- data.frame(region=character(),
                           landleaf=character(),year=integer(),
                           land_alloc=double())
    colnames(data) <- table_cols
    # find all leaves in a single land root level and make sure this also gets
    # unmanaged:
    #
    #   Notes on why we have if statements:
    #   Have to treat land inputs 2 and 3 specially for the protected lands case.
    #   HAve to keep some but not all of the entries in the original land_inputs file
    #   and it's manual and ugly but it works. If we ever dramatically change
    #   how GCAM does protected lands or how many land nests there are, this will
    #   have to be recoded.
    
    if( i == 2 & length(land_roots) == 7){
      
      orig_leaf <- xml2::xml_find_all(land_roots[[i]],"//LandLeaf")
      orig_unmgd <- xml2::xml_find_all(land_roots[[i]], "//UnmanagedLandLeaf")
      
      # there are no protected LandLeaf entries, only protected UnmanagedLandLeaf:
      prot_unmgd <- xml2::xml_find_all(land_roots[[6]], "//UnmanagedLandLeaf")
      
      
      # For land input 2, straightforward. We just do leaf and and protected
      # unmanaged. We can tell this from assumptions in the data system
      #TODO integrate this code directly with data system so have access to these
      # to be able to make more robust and less hardcoded?
      # https://github.com/JGCRI/gcamdata/blob/main/inst/extdata/aglu/A_LandLeaf2.csv
      # https://github.com/JGCRI/gcamdata/blob/main/inst/extdata/aglu/A_LandLeaf_Unmgd2.csv
      # The only Level 2 unmanaged land leaf is UnmanagedPasture, and that
      # does become protected
      all_leaves <- c(orig_leaf, prot_unmgd)
      
      
    }else if(i==3 & length(land_roots) == 7){
      orig_leaf <- xml2::xml_find_all(land_roots[[i]],"//LandLeaf")
      orig_unmgd <- xml2::xml_find_all(land_roots[[i]], "//UnmanagedLandLeaf")
      
      # there are no protected LandLeaf entries, only protected UnmanagedLandLeaf:
      prot_unmgd <- xml2::xml_find_all(land_roots[[7]], "//UnmanagedLandLeaf")
      
      
      # Level 3 is even more hardcoded/relying un expertise, unfortunately.
      # https://github.com/JGCRI/gcamdata/blob/main/inst/extdata/aglu/A_LandLeaf3.csv
      # https://github.com/JGCRI/gcamdata/blob/main/inst/extdata/aglu/A_LandLeaf_Unmgd3.csv
      #
      # Just like with level 2, we want he LandLeaf files as is.
      # Unlike Level 2, There are 4 kinds of unmanaged land leaf instead of just 1:
      # UnmanagedForest
      # Shrubland
      # Grassland
      # OtherArableLand
      
      # UnmanagedForest,Shrubland and Grassland all have protected versions,
      # so we just want to read the prot_unmgd for them.
      # But there is no protected OtherArableLand. So we want to keep
      # just the OtherArableLand entries from orig_unmanaged
      #
      
      all_leaves <- c(orig_leaf,
                      orig_unmgd[grepl('OtherArableLand', orig_unmgd)],
                      prot_unmgd)
      
    }else{
      all_leaves <- xml2::xml_find_all(land_roots[[i]],"//LandLeaf")
      unmanaged_leaves <- xml2::xml_find_all(land_roots[[i]], "//UnmanagedLandLeaf")
      all_leaves <- c(all_leaves, unmanaged_leaves)
      
    } # End 'find all leaves in a single land root level '
    
    leaf_count <- 0

    # TODO for efficiency: run leaves in batches of 250 at a time, then add 250 on to main database and redefine data
    for (leaf in all_leaves){
      
      # print('===================================')
      # print(xml2::xml_attr(leaf,"name"))
      # print(count+1)
      # print(i)
      # print('===================================')
      
      new_leaf_data <- process_leaf(leaf,gcam_land_alloc) 
      count <- count+1
      idx <- count+nrows-1
      data <- dplyr::bind_rows(data,new_leaf_data)  # TODO update to rbindlist
      leaf_count <- leaf_count + 1

      if (leaf_count == 250){
        print(c(i, leaf_count, count))
        all_data <- dplyr::bind_rows(all_data,data)
        data <- data.frame(region=character(),
                           landleaf=character(),year=integer(),
                           land_alloc=double())
        colnames(data) <- table_cols
        leaf_count <- 0
      } # End if leaf_count == 250

    } # End for leaf in all_leaves
    all_data <- dplyr::bind_rows(all_data,data)  # bind remaining leaves that have not been covered already
  }  # end for i in 1:5
  return(all_data)
}

process_leaf <- function(leaf_node, gcam_land_alloc){
  name <- xml2::xml_attr(leaf_node,"name")
  region <- get_region(leaf_node)

  land_alloc <- get_leaf_land_alloc(leaf_node, name, region, gcam_land_alloc)

  leaf_output <- data.frame(region={{region}}, landleaf={{name}}, year=land_alloc$year, land_alloc=land_alloc$value)

  return(leaf_output)
}

get_region <- function(node,la_str="/LandAllocatorRoot"){
  reg_path <- strsplit(xml2::xml_path(node),la_str)[[1]][1]
  reg_node <- xml2::xml_find_first(node,reg_path)
  reg_name <- xml2::xml_attr(reg_node,"name")
  return(reg_name)
}

parse_c_densities <- function(leaf_data, years){
  above_grnd <- as.numeric(leaf_data$`land-use-history`$`above-ground-carbon-density`)
  below_grnd <- as.numeric(leaf_data$`land-use-history`$`below-ground-carbon-density`)
  if (length(above_grnd)==0){
    return(NULL)
  } else {
    return(data.frame(year=years,above_ground=above_grnd, below_ground=below_grnd))
  }
}

get_leaf_land_alloc <- function(leaf_node, leaf_name, leaf_region, gcam_land_alloc){
  
  # Deal with land allocation info from the xml inputs first:
  leaf_data <- xml2::as_list(leaf_node)  # convert leaf data from xml into
                                         # something parseable in R
  land_alloc_df <- parse_land_alloc(leaf_data)  # get the historical land 
                                                # allocation data from the xmls
  # ^ includes if have a leaf with 0 historical land allocation 
  # (e.g. OtherGrainC4_NelsonR_IRR hi and lo), there are no 1975-2015 
  # `landAllocation` quantities in the XML - instead of giving NA values in the
  # parse_land_alloc() function, give values of 0 for these years in this case.

  
  # The land allocation info from the gcam data base:
  gcam_leaf_land_alloc <- get_gcam_land_alloc_by_leaf(leaf_region=leaf_region, 
                                                      leaf_name=leaf_name, 
                                                      gcam_alloc=gcam_land_alloc)

  if(nrow(gcam_leaf_land_alloc) == 0){
    message(paste('This land leaf', leaf_name, leaf_region, 
                  '\nexists in the land input xml files but not the gcam output 
                  \ndatabase land allocation. If land input XML allocation for 
                  \nhistorical years all 0, then will the this leaf will be 
                  \nassigned 0 land allocation for all years modeled by GCAM.'))
  } # we know it exists in the xml because it had to for us to get into this
    # function. SO far, we have never seen a case where it exists with non-0
    # allocation in the input XML but somehow isn't present in gcam output db.
    # that would be a bigger gcam error.
  
  # GCAM input XMLs are going to cover 1700-2015 (in very large time steps).
  # GCAM output data base land allocations cover 1975-2100 (in diff time steps).
  #
  # We must merge these together to get 1700-2100:
  #
  # First, find all years of overlap between modeled (GCAM_database) and
  #  historical (GCAM input XML) land alloc and remove from historical
  
  # pull off the first model year from the gcam data base
  first_model_year <- gcam_leaf_land_alloc$year[1]

  # if doesn't exist in the GCAM data base but does in XML with all 0 historical
  # (1700-1950) allocation, then Just need to make 0 for all of the years to 2100. 
  if(is.na(first_model_year) & max(abs(land_alloc_df$value), na.rm = T) == 0){
    
    # last year from the xml inputs:
    max_inputxml_yr <- max(land_alloc_df$year)
    
    # based on years other land leafs get in the gcam_land_alloc from the output
    # DB, make a vector of years after max_inputxml_yr to end of GCAM run (2100)
    tmp_yrs <- (gcam_land_alloc %>% 
                  select(year) %>% 
                  distinct() %>%
                  filter(year > max_inputxml_yr))$year

    land_alloc_all_years <- rbind(land_alloc_df,
                                  data.frame(year = tmp_yrs,
                                             value = 0)) %>% distinct() # makes sure not having years twice.
    rm(tmp_yrs)
    
  }else if (!is.na(first_model_year)){

    # take the model year from the gcamdata base, and  match in to land_alloc_df,
    # which comes from the XML inputs.
    idx <- match(first_model_year, land_alloc_df$year)
    # remove any land allocation from the historical that's covered by gcam
    # database output:
    land_alloc_df <- land_alloc_df[1:idx-1,]  
    
    land_alloc_all_years <- rbind(land_alloc_df,gcam_leaf_land_alloc)
  }
  
  # clean up row names
  rownames(land_alloc_all_years) <- 1:length(land_alloc_all_years$year)
  
  # interpolate land allocation data - currently linear. Spline was being weird.
  all_years <- seq(min(land_alloc_all_years$year), max(land_alloc_all_years$year), 1)
  land_alloc_interp <- approx(land_alloc_all_years$year,land_alloc_all_years$value, xout=all_years, ties="ordered")
  land_alloc_interp_df <- as.data.frame(land_alloc_interp)  # approx returns named list with names x and y
  colnames(land_alloc_interp_df) <- c("year", "value")
  return(land_alloc_interp_df)
}


parse_land_alloc <- function(leaf_data){
  # get historical land allocation
  hist_listed_alloc <- leaf_data$`land-use-history`[names(leaf_data$`land-use-history`)=='allocation']
  n_hist <- length(hist_listed_alloc)
  year_hist <- numeric(n_hist)
  value_hist <- numeric(n_hist)
  
  for (i in 1:n_hist){
    year_hist[i] <- as.numeric(attributes(hist_listed_alloc[[i]])$year) # this is specific to exact format of current land input xmls
    value_hist[i] <- as.numeric(hist_listed_alloc[[i]][[1]])
  }
  hist_df <- data.frame(year=year_hist, value=value_hist)
  hist_df <- hist_df[order(hist_df$year),]  # population[order(population$age),]
  
  # get modern land allocation if it exists
  listed_alloc <- leaf_data[names(leaf_data)=='landAllocation']
  n_mdrn <- length(listed_alloc)
  
  mdrn_years <- c(1975, 1990, 2005, 2010, 2015)
  
  mdrn_df <- data.frame(year=mdrn_years,value=NA)
  
  if (n_mdrn!=0){
    for (i in 1:n_mdrn){
      data_year <- as.numeric(attributes(listed_alloc[[i]])$year) # this is specific to exact format of current land input xmls
      data_value <- as.numeric(listed_alloc[[i]][[1]])
      if (data_year %in% mdrn_df$year){
        mdrn_df[mdrn_df$year==data_year,] <- c(data_year,data_value)
      }
    }
  } else if (max(abs(hist_df$value)) <1e-6 & n_mdrn==0){
    mdrn_df <- data.frame(year=mdrn_years,value=0)
  }
  hist_df <- hist_df[1:(length(hist_df$year)-1),]  # remove 1975 overlap
  return(data.frame(year=c(hist_df$year,mdrn_df$year),value=c(hist_df$value,mdrn_df$value)))
  
}



get_leaf_params <- function(land_roots, soilTimeScales, land_alloc_data, data_names = c("above-ground-carbon-density","below-ground-carbon-density","mature-age")
){
  
  leaf_params <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
  colnames(leaf_params) <- c("region","landleaf",data_names)
  count <- 0
  for (root in land_roots){
    count <- count + 1
    print("STARTING NEXT ROOT")
    print(count)
    tmp <- get_data_byLeaf2(root, data_names)
    leaf_params <- data.table::rbindlist(list(leaf_params,tmp))
  }
  head(leaf_params)
  
  leaf_params <- dplyr::mutate(leaf_params,agCarbon0=0.0,bgCarbon0=0.0,npp_factor=0.0,NPP0=0.0, co20=0.0)
  leaf_params <- dplyr::left_join(leaf_params,soilTimeScales,by="region")
  
  dplyr::filter(land_alloc_data,year==1700) %>% dplyr::mutate(land0=land_alloc) %>% dplyr::select(-c("year","land_alloc")) -> land0_df
  leaf_params <- dplyr::left_join(leaf_params,land0_df,by=c("region","landleaf"))
  
  for (col in colnames(leaf_params)[3:10]){
    leaf_params[[col]] <- as.numeric(leaf_params[[col]])
  }
  leaf_params$`above-ground-carbon-density` <- 0.2*leaf_params$`below-ground-carbon-density`
  leaf_params$name <- paste(leaf_params$region, leaf_params$landleaf, sep="_")
  
  # store leaf_data so it's easily accessible
  saveRDS(leaf_params,file="data/leaf_params.RDS")
  return(leaf_params)
}


get_data_byLeaf2 <- function(root_node, data_names){
  data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
  colnames(data) <- c("region","landleaf",data_names)
  
  all_regions <- xml2::xml_find_all(root_node,"//region")
  for (region in all_regions){
    reg_name <- xml2::xml_attr(region,"name")
    print(reg_name)
    all_leaves <- xml2::xml_find_all(region,paste0(xml2::xml_path(region),"//LandLeaf|",
                                                   xml2::xml_path(region),"//UnmanagedLandLeaf"))
    count <- 1
    tmp_data <- data.frame(matrix(ncol=2+length(data_names),nrow=length(all_leaves)))
    colnames(tmp_data) <- c("region","landleaf",data_names)
    #print(all_leaves)
    for (leaf in all_leaves){
      leaf_name <- xml2::xml_attr(leaf,"name")
      print(paste0(reg_name,", ",leaf_name))
      new_row <- c(reg_name,leaf_name)
      #row_names <- c("region","landleaf")
      for (data_name in data_names){
        sts_node <- xml2::xml_find_first(leaf,paste0(xml2::xml_path(leaf),"/land-carbon-densities/",
                                                     data_name,"|",xml2::xml_path(leaf),"/no-emiss-carbon-calc/",data_name))  # don't necessarily need the land-carbon-densities part.
        sts <- as.numeric(xml2::xml_text(sts_node))
        new_row <- c(new_row, sts)
        #row_names <- c(row_names,data_name)
      }
      #names(new_row) <- row_names
      #print(new_row)
      tmp_data[count,] <- new_row
      count <- count + 1
    }
    data <- data.table::rbindlist(list(data,tmp_data))
  }
  names(data) <- c("region","landleaf",data_names)
  return(data)
}

get_data_byLeaf <- function(root_node, data_names){
  data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
  colnames(data) <- c("region","landleaf",data_names)
  
  all_regions <- xml2::xml_find_all(root_node,"//region")
  for (region in all_regions){
    reg_name <- xml2::xml_attr(region,"name")
    print(reg_name)
    all_leaves <- xml2::xml_find_all(region,paste0(xml2::xml_path(region),"//LandLeaf|",xml2::xml_path(region),"//UnmanagedLandLeaf"))
    count <- 0
    tmp_data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
    colnames(tmp_data) <- c("region","landleaf",data_names)
    #print(all_leaves)
    for (leaf in all_leaves){
      leaf_name <- xml2::xml_attr(leaf,"name")
      #print(paste0(reg_name,", ",leaf_name))
      new_row <- c(reg_name,leaf_name)
      #row_names <- c("region","landleaf")
      for (data_name in data_names){
        sts_node <- xml2::xml_find_first(leaf,paste0(xml2::xml_path(leaf),"/land-carbon-densities/",data_name,"|",xml2::xml_path(leaf),"/no-emiss-carbon-calc/",data_name))  # don't necessarily need the land-carbon-densities part.
        sts <- as.numeric(xml2::xml_text(sts_node))
        new_row <- c(new_row, sts)
      }
      tmp_data <- data.table::rbindlist(list(tmp_data,new_row))
      count <- count + 1
      if (count == 100){
        data <- data.table::rbindlist(list(data,tmp_data))
        tmp_data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
        colnames(tmp_data) <- c("region","landleaf",data_names)
        count <- 0
      }
      
    }
  }
  names(data) <- c("region","landleaf",data_names)
  return(data)
}


get_soilTS_byRegion <- function(root_node){
  data <- data.frame(matrix(ncol=2,nrow=0))
  colnames(data) <- c("region","soilTimeScale")
  
  all_regions <- xml2::xml_find_all(root_node,"//region")
  for (region in all_regions){
    reg_name <- xml2::xml_attr(region,"name")
    sts_node <- xml2::xml_find_first(region,paste0(xml2::xml_path(region),"/LandAllocatorRoot/soilTimeScale"))
    sts <- as.numeric(xml2::xml_text(sts_node))
    new_row <- c("region"=reg_name, "soilTimeScale"=sts)
    data <- rbind(data,new_row)
  }
  colnames(data) <- c("region","soilTimeScale")
  return(data)
}

#ATTENTION
###File paths here!
get_gcam_land_alloc <- function(db_name="database_basexdbGCAM", gcam_dir="pic_data/pic_hist_base_DB/", scenario="Reference", read_from_file=FALSE, filename="data/gcam_land_alloc.csv"){
  
  if (read_from_file) {
    gcam_land_alloc <- read.csv2(file=filename,header=TRUE)
  }
  else {
    base_conn <- rgcam::localDBConn(gcam_dir, db_name)
    
    land_alloc_query <- '<query title="detailed land allocation">
    <axis1 name="LandLeaf">LandLeaf[@name]</axis1>
    <axis2 name="Year">land-allocation[@year]</axis2>
    <xPath group="false" sumAll="false" buildList="true" dataName="LandLeaf">/LandNode[@name=\'root\' or @type=\'LandNode\' (:collapse:)]//land-allocation/text()</xPath>
    <comments/>
</query>'
    
    new.proj <- rgcam::addSingleQuery(base_conn, "new.proj", "Land Allocation", land_alloc_query, c(scenario), clobber=TRUE)
    
    gcam_land_alloc <- rgcam::getQuery(new.proj, "Land Allocation")
    write.csv2(gcam_land_alloc, file=filename, row.names = FALSE)
  }
  
  return(gcam_land_alloc)
}

get_gcam_land_alloc_by_leaf <- function(leaf_region, leaf_name, gcam_alloc){
  leaf_land_alloc <- gcam_land_alloc[gcam_alloc$region==leaf_region & gcam_alloc$landleaf==leaf_name,]
  leaf_land_alloc <- leaf_land_alloc[,c("year", "value")]  # match columns from historical xml data
  return(leaf_land_alloc)
}

#ATTENTION
###File paths here!
#creates an empty project
get_gcam_emissions <- function(db_name="database_basexdb", gcam_dir="reference"){
  base_conn <- localDBConn(gcam_dir, db_name)
  luc_query <- '<query title="Land Use Change Emission">
         <axis1 name="land-use-change-emission">LandLeaf</axis1>
         <axis2 name="Year">land-use-change-emission[@year]</axis2>
         <xPath buildList="true" dataName="land-use-change-emission" group="false" sumAll="true">/LandNode[@name=\'root\' or @type=\'LandNode\' (: collapse :)]//land-use-change-emission/text()</xPath>
         <comments/>
      </query>'
#TODO can this be deleted?  
  new.proj <- addSingleQuery(base_conn, "new.proj", "Land Use Change Emissions", luc_query, c("Reference"), clobber=TRUE)
  
  gcam_luc <- getQuery(new.proj, "Land Use Change Emissions")
  return(gcam_luc)
}

