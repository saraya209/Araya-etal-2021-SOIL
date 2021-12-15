## Collection of functions to read HYDRUS-2D output files

read.H2D.out <- function(project.path, out.path, project.id){
  ## Function to read Hydrus-2 .out files and convert them to anaysis
  ## friendly tables.
  ## - T-level information (h_mean, v_mean, cum_q, run_inf):Information printed 
  ##                                       at end of each calculation time step
  ## - P-level information (h.out, th.out...BALANCE.OUT): Information printed 
  ##                                           ONLY at prescribed "print times"
  ## - A_LEVEL.OUT: information printed each time a time dependent boundary is 
  ##                specified (i.e hourly if variable condition table is hourly.)
  
  ### read A_LEVEL.OUT: Pressure heads and cumulative fluxes on the boundary and in the root zone
  
  alevel.data = read_lines(file = file.path(project.path,"A_Level.out"))
  
  # trim non table component
  start_ind = grep("Time", alevel.data)
  end_ind = grep("end", alevel.data) -1
  
  alevel.data = alevel.data[start_ind:end_ind]
  
  # Convert columns (variable number of spaces) to commas = csv file.
  alevel.data = alevel.data %>% 
    str_squish() %>% # remove consecutive spaces
    str_replace_all(fixed(" "), ",") # replace space with comma
  
  #head(alevel.data) 
  
  # modify header and read as csv
  acol_header = read_csv(alevel.data[1:2], col_names = F)
  acol_header = as.character(acol_header[1,])
  
  alevel.dt = read_csv(alevel.data, col_names = acol_header, skip = 3)
  
  alevel.dt = alevel.dt%>%
    mutate(Plot_ID = project.id)
  
  acol_header = colnames(alevel.dt)
  
  aout.name = file.path(out.path,"A_Level.csv")
  
  if(!file.exists(aout.name)){
    # Write column names if the first instance, else just data
    write_csv(acol_header, path = aout.name, col_names = F)
  }
  write_csv(alevel.dt, path = aout.name,
            append = T, col_names = F
  )
  
  ### read T_LEVEL.OUT. Pressure heads and cumulative fluxes on the boundary and in the root zone
  
  n.max = length(read_lines(file = file.path(project.path,"T_Level.out"))) - 10
  
  tlevel.dt = read_table(file = file.path(project.path,"T_Level.out"), 
                         skip = 9, col_names = F,
                         n_max = n.max)
  
  ##tlevel.dt = dplyr::select(tlevel.dt, -X1) ## X1 is time?
  
  tlevel.dt = tlevel.dt%>%
    mutate(Plot_ID = project.id)
  
  tcol_header = data.frame("Time", "r_Top", "r_Root", "v_Top", "v_Root", "v_Bot", 
                           "sum_r_Top", "sum_r_Root","sum_v_Top","sum_v_Root", "sum_v_Bot", 
                           "h_Top","h_Root","h_Bot", "Runoff", "sum_Runoff", "Volume",
                           "sum_Infil", "sum_Evap", "TLevel","cum_WTrans", "Snowlayer","Plot_ID")
  
  tout.name = file.path(out.path,"T_Level.csv")
  
  if(!file.exists(tout.name)){
    write_csv(tcol_header, path = tout.name, col_names = F)
  }
  
  write_csv(tlevel.dt, path = tout.name,
            append = T, col_names = F
  )
  
}


read.h2d.alevel.out <- function(project.id, parent.dir, project.folder){
  ## Function to read Hydrus-2 .out files and convert them to analysis
  ## friendly tables.
  ## - A_LEVEL.OUT: information printed each time a time dependent boundary is 
  ##                specified (i.e hourly if variable condition table is hourly.)
  
  ### read A_LEVEL.OUT: Pressure heads and cumulative fluxes on the boundary and in the root zone
  project.name = paste(project.folder, project.id, sep = "_")
  project.path = path.expand(file.path(parent.dir, project.name))
  
  alevel.data = read_lines(file = file.path(project.path,"A_Level.out"))
  
  # trim non table component
  start_ind = grep("Time", alevel.data)
  end_ind = grep("end", alevel.data) -1
  
  alevel.data = alevel.data[start_ind:end_ind]
  
  # Convert columns (variable number of spaces) to commas = csv file.
  alevel.data = alevel.data %>% 
    str_squish() %>% # remove consecutive spaces
    str_replace_all(fixed(" "), ",") # replace space with comma
  
  #head(alevel.data) 
  
  # modify the 2-line header into one
  aheader = str_split(alevel.data[1:2], pattern = ",", simplify = T)
  aheader = paste0(aheader[1,], aheader[2,]) %>% 
    janitor::make_clean_names("parsed")
  
  # Read A-Level data as csv with new header
  alevel.dt = read_csv(alevel.data, col_names = aheader, skip = 3)
  
  alevel.dt = alevel.dt%>%
    mutate(Plot_ID = project.id)
  
  return(alevel.dt)
  
}


read.h2d.balance.out <- function(project.id, parent.dir, project.folder, sub.regions = 6){
  ## Function to read Hydrus-2 .out files and convert them to analysis
  ## friendly tables.
  ## BALANCE.OUT: Information printed ONLY at prescribed "print times" for each sub-regions
  
  ## read Balance.out. Mass balance variables
  project.name = paste(project.folder, project.id, sep = "_")
  project.path = path.expand(file.path(parent.dir, project.name))
  
  balance_out = read_lines(file = file.path(project.path, "Balance.out"), skip = 9)
  
  # Convert columns (variable number of spaces) to commas = csv file.
  balance_out = balance_out %>% 
    str_squish() %>% # remove consecutive spaces
    str_replace_all(fixed(" "), ",") # replace space with comma
  # head(balance.out)
  # tail(balance.out)
  
  # Grab lines
  top_ind = grep("Volume,", balance_out) - 1 # Start of table
  # time_ind = grep("Time  ", balance.out) - 1
  bot_ind = grep("Yield,", balance_out) # End of table
  # bot_ind = c(17,bot_ind) # append the end of the first table
  
  ## Create block table header
  
  reg_head = 0:sub.regions
  reg_head = paste0("Layer_", reg_head)
  # Create a header 
  block_head = c("Variable", "Units", reg_head)
  
  ## Read tables and merge into one
  
  count_times = 1:length(top_ind)
  
  
  read.i.balance = function(count.times = count_times,
                            top.ind = top_ind, 
                            bot.ind = bot_ind,
                            balance.out = balance_out,
                            block.head = block_head){
    # Set n-th parameters
    top.ind = top.ind[count.times]
    bot.ind = bot.ind[count.times]
    time.ind = top.ind - 4
    n.max = bot.ind - top.ind
    # read table
    block_i = read_csv(balance.out,
                       col_names = block.head,
                       guess_max = 2,
                       skip = top.ind,
                       n_max = n.max,
                       col_types = cols())
    # read time
    time_i = read_csv(balance.out,
                      col_names = F,
                      skip = time.ind,
                      n_max = 1)
    time_i = as.numeric(time_i[1,3])
    
    # Append time
    block_i = block_i %>%
      mutate(Time = time_i)
    #merge tables
    
    return(block_i)
    
  }
  
  block_i.dt.lst = lapply(count_times, read.i.balance)
  
  balance.dt = bind_rows(block_i.dt.lst)
  
  
  # Finalize plot table
  balance.dt = balance.dt%>%
    #dplyr::rename(balance_head) %>% 
    mutate(Plot_ID = project.id)
  
  return(balance.dt)
  
}


#' ## Read Hydrus `.out` Binary Files
#' 
#' The binary files are read as a single string of numbers.
#' 
#' For each timestep (0 to 3360 hours = 3361 time points)
#' a grid containing 1473 points is produced.
#' Before the start of each grid the number is the time
#' there fore the total number of digits read = 
#' 1473 * 3361 = 4950753
#' 
#' Plus one digit every 1473 for the time =
#' 4950753 + 3361 = 4954114

#' Function to read in binary out files:
read.h2d.mesh.out <- function(project.id, parent.dir, project.folder, out_type = "th.out"){
  ## Function to read Hydrus-2 .out files and convert them to analysis
  ## friendly tables.
  ## - A_LEVEL.OUT: information printed each time a time dependent boundary is specified (i.e hourly if variable condition table is hourly.)
  
  ### read A_LEVEL.OUT: Pressure heads and cumulative fluxes on the boundary and in the root zone
  project.name = paste(project.folder, project.id, sep = "_")
  project.path = path.expand(file.path(parent.dir, project.name))
  
  h.out = readBin(file.path(project.path, out_type) , numeric(), n = 5959360, size = 4)
  
  time_ind = seq(1, 3361*1474, by = 1474)
  
  #function to split vector by time
  split_to_table = function(ind, x, x_len = 1473){
    t = x[ind]
    start_ind = ind + 1
    end_ind = ind + x_len -1
    d = x[start_ind:end_ind]
    d = tibble(h = d) %>% 
      dplyr::mutate(Time = t,
                    id = row_number())
    return(d)
  }
  
  h.lst = lapply(time_ind, split_to_table, x = h.out)
  h.dt = bind_rows(h.lst)
  
  h.dt = h.dt %>% 
    left_join(mesh.dt, by = "id") %>% 
    dplyr::mutate(Plot_ID = project.id)
  
  
  
  return(h.dt)
  
}

