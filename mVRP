RandomBreaks <- function(num_agents, n, min_tour, max_tour_visits = 5){


  if(num_agents * max_tour_visits < n){
    stop('Number of points exceeds capacity. Stopping optimizer...')
  }

  if(min_tour < 2){
    stop('An agent cannot have less than 2 entries in the tour solution. Two entries signify that the agent did not make any visits')
  }

  if(min_tour > max_tour_visits){
    stop("The minimum tour length selected is higher than the max tour length. Please correct your inputs.")
  }

  min_num_required <-  ceiling(n / max_tour_visits)

  if(num_agents > 2){
    if(num_agents * max_tour_visits <= n & num_agents * max_tour_visits >= n - max_tour_visits){
      brks <- round(seq(from = max_tour_visits, to = n - max_tour_visits, length.out = num_agents - 1))

    } else{
      num_brks <- num_agents
      M        <- zoo::rollapply(data = seq(from = 1, to = n, length.out = num_agents), 2, by = 1, c)
      for (i in 1:nrow(M)) {M[i, 1] <- M[i, 1] + 1}

      brks <- c()
      for (i in 1:nrow(M)) {
        brks[i] <- round(runif(n = 1, min = M[i, 1], max = M[i, 2]))
      }

      for (i in 1:(length(brks) - 1)) {

        if(abs(brks[i + 1] - brks[i]) > max_tour_visits){
          spread <- brks[i + 1] - brks[i] - max_tour_visits
          brks[i + 1] <- brks[i + 1] - spread

        }
      }


      L <- length(brks)
      if(n - brks[L] > max_tour_visits &  brks[L] - brks[L-1] < max_tour_visits){
        d1 <- brks[L] - brks[L - 1]
        d2 <- n - brks[L]
        brks[L] <- brks[L] + (d2 - d1)

      }

      if(brks[L] == n){
        brks[L] <- floor((brks[L - 1] + n)/2)
      }

    }
  } else if(num_agents == 2){

    brks <- max_tour_visits - sample(x = 0:2, size = 1)

  }

  return(brks[!is.na(brks)])

}


CalculateRange <- function(point_break, n){
  flag  <- 1
  range <- matrix(data = 0, nrow = length(point_break) + 1, ncol = 2)

  for (i in 1:length(point_break)){
    if (flag == 1 & point_break[i]>1){
        range[i, ] <- c(1, point_break[i])
        flag <- 0
    } else if (flag == 1){
        range[i, ] <- c(1, 0)
    } else if (point_break[i] <= point_break[i - 1]){
        range[i, ] <- c(point_break[i - 1], point_break[i])
    } else if (i < length(point_break)){
        range[i, ] <- c(point_break[i - 1] + 1, point_break[i])
    } else {
        range[i, ] <- c(point_break[i - 1] + 1, point_break[i])
    }
  }

  if (point_break[length(point_break)] < n && point_break[length(point_break)]!=1){
      range[i + 1, ] <- c(point_break[length(range)] + 1, n)
  } else if (point_break[length(point_break)]<n && point_break[length(point_break)]==1){
      range[i + 1,] <- c(point_break[length(range)], n)
  } else {
      range[i + 1,] <- c(point_break[length(range)], n - 1)
  }

  if(range[i, 2] == 1){
      range[i + 1, 1] <- 1
  } else if(range[i, 2] == n){
      range[i + 1, 1] <- n
  } else {
      range[i + 1, 1] <- range[i, 2] + 1
  }

  return(range)
}


CalculateTourLength <- function(tour = NULL,
                                dist_mat = NULL,
                                dist_mat0 = NULL,
                                indices = NULL,
                                max_tour_len = 100,
                                max_num_visits = 5,
                                penalty = 5){

  VehicleTourLength <- dist_mat0[tour[1], tour[2]]

  for (i in 2:(indices - 1)) {
    VehicleTourLength <- VehicleTourLength + dist_mat[tour[i + 1], tour[i]]
  }

  VehicleTourLength <- VehicleTourLength + dist_mat0[tour[indices + 1], tour[indices]]

  if(VehicleTourLength > max_tour_len){
    VehicleTourLength <- VehicleTourLength * penalty
  }

  if(length(tour) - 2 >  max_num_visits){
    VehicleTourLength <- VehicleTourLength * penalty
  }

  return(VehicleTourLength)
}

VehicleRouting  <- function(visit_points = NULL,
                            num_agents = NULL,
                            agent_points = NULL,
                            cost_type = 2,
                            distance_matrix = NULL,
                            distance_metric = 'Geodesic',
                            max_tour_visits = NULL,
                            max_tour_distance = inf,
                            min_tour = 2,
                            population_size = 96,
                            num_generations = 100,
                            penalty_factor  = 5,
                            distance_truncation = FALSE,
                            seed = 42){

  set.seed(seed)

  if(is.null(visit_points)){
    stop('No data provided for the visit points. Stopping...')
  }
  if(is.null(agent_points)){
    stop('No data provided for the agent positions. Stopping...')
  }
  if(isFALSE(inherits(x = visit_points, what = 'matrix'))){
    stop('Please provide a matrix object with the visit_points coordinates')
  }
  if(isFALSE(inherits(x = agent_points, what = 'matrix'))){
    stop('Please provide a matrix object with the agent coordinates')
  }
  if(num_generations <= 2){
    stop('Please use more than 2 generations for the optimizer. Recommending at least 1000 generations for problems with 2-3 agents, and +3000 for problems with +10 agents.')
  }
  if(num_agents < 2){
    stop('The procedure requires at least two agents to solve the problem. For a single agent, please see the traveling salesman problem.')
  }
  if(max_tour_distance <= 0){
    stop('Please provide a positive value for the maximum tour distance travelled.')
  }
  if(max_tour_visits <= 0){
    stop('Please provide a positive integer value for the max_tour_visits capacity parameter.')

  }
  if(isFALSE(distance_metric %in% c('Euclidean', 'Geodesic'))){
    stop('Please select a valid distance metric. Valid options are euclidean and geodesic distance.')
  }
  if(isFALSE(is.numeric(penalty_factor))){
    stop('Please provide a numeric value for the penalty factor used for imposing mileage constraints.')
  }
  if(max_tour_visits > nrow(visit_points)){
    message('The specified max_tour_visits per agent is greater than the number of all visit points. \n
            Reducing to the number of visit points.')
    max_tour_visits = nrow(visit_points)
  }


  # START TEST
  # Distance Truncation (Nearest caretaker)
  if(distance_truncation == TRUE){

    n <- nrow(visit_points)
    visit_points = as.data.frame(visit_points)
    agent_points = as.data.frame(agent_points)
    visit_points$distance_to_nearest_agent <- NA


    for (i in 1:n) {
      df <- data.frame(Longitude = c(visit_points$lon[i],  agent_points$lon),
                       Latitude  = c(visit_points$lat[i],  agent_points$lat))

      dist <-  geodist::geodist(x = df, measure = 'haversine')/1000
      visit_points$distance_to_nearest_agent[i] <- as.numeric(sort(dist[, 1])[2])

    }

    # Eliminate patients who are too far from their nearest caretaker
    visit_points <-
      visit_points %>%
      dplyr::filter(distance_to_nearest_agent < max_tour_distance) %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::select(-distance_to_nearest_agent) %>% as.matrix()

    agent_points = as.matrix(agent_points)

  }


  # END TEST


  # The minimum number of caretakers required to satisfy the demand for services.
  capacity  <- num_agents * max_tour_visits
  npatients <- nrow(visit_points)
  min_req_caretakers <- ceiling(nrow(visit_points)/max_tour_visits)
  epsilon <- 1e-10

  problem_dimension <- dim(visit_points)
  num_visit_points  <- problem_dimension[1]
  D0                <- matrix(data = 0, nrow = num_agents, ncol = num_visit_points)

  if(distance_metric == 'Euclidean'){
    for (i in 1:num_agents) {
      for (j in 1:num_visit_points) {
        D0[i, j] <- norm(as.matrix(agent_points[i, ] - visit_points[j, ]))
      }
    }
  } else if(distance_metric == 'Geodesic'){
    for (i in 1:num_agents) {
      for (j in 1:num_visit_points) {
        D0[i, j] <- geodist::geodist(agent_points[i, ],  visit_points[j, ], measure = 'haversine')/1000
      }
    }
  }



  n    <- problem_dimension[1]
  dims <- length(dim(problem_dimension))
  nr <- dim(distance_matrix)[1]
  nc <- dim(distance_matrix)[2]


  min_tour        <- max(1, min(n, round(min_tour)))
  population_size <- max(8, 8*ceiling(population_size[1]/8))
  num_generations <- max(1, round(num_generations[1]))


  pop_rte <- matrix(data = 0, nrow = population_size, ncol = n)
  pop_brk <- matlab::cell(population_size, 1)

  for (k in 1:population_size) {
    pop_rte[k, ] <- pracma::randperm(n)
    pop_brk[[k]] <- RandomBreaks(num_agents = num_agents, n = n, min_tour = min_tour, max_tour_visits = max_tour_visits)

  }

  # Define a global minima to be optimized. Use infinity by default.
  globalMin = Inf

  total_dist   <- matrix(data = 0, nrow = 1, ncol = population_size)
  dist_history <- matrix(data = 0, nrow = 1, ncol = num_generations)
  tmp_pop_rte  <- matrix(data = 0, nrow = 16, ncol = n)
  tmp_pop_brk  <- matlab::cell(16, 1)
  new_pop_rte  <- matrix(data = 0, nrow = population_size, ncol = n)
  new_pop_brk  <- matlab::cell(population_size, 1)

  # Start the optimizer
  iter      <- 0
  iter_left <- 0

  while (iter < num_generations) {

    iter_left <- iter_left + 1
    iter      <- iter + 1

    for (p in 1:population_size) {

      p_rte       <- pop_rte[p, ]
      point_break <- pop_brk[[p]]
      caretaker   <- length(point_break) + 1
      d     <- numeric(length = caretaker)
      range <- CalculateRange(point_break, n)

      for (sa in 1:caretaker) {
        if(range[sa, 1] <= range[sa, 2]){
          Tour    <- c(sa, p_rte[range[sa, 1] : range[sa, 2]], sa)
          indices <- length(Tour) - 1
          d[sa]   <- CalculateTourLength(tour = Tour,
                                         dist_mat = distance_matrix,
                                         dist_mat0 = D0,
                                         indices = indices,
                                         max_tour_len = max_tour_distance,
                                         max_num_visits = max_tour_visits,
                                         penalty = penalty_factor)
        } else{
          Tour  <- c(sa, sa)
          d[sa] <- 0
        }


      }

      if (cost_type == 1) {
        total_dist[p] <- sum(d)
      } else if(cost_type == 2){
        total_dist[p] <- max(d) + epsilon * sum(d)
        tour_lengths  <- d
      }

    }

    min_dist <- min(total_dist)
    index    <- which.min(total_dist)
    dist_history[iter] <- min_dist


    if(min_dist < globalMin){
      iter_left  <- 0
      generation <- iter
      global_min <- min_dist
      opt_route  <- pop_rte[index, ]
      opt_brk    <- pop_brk[[index]]
      caretaker  <- length(opt_brk) + 1
      range      <- CalculateRange(opt_brk, n)

    }

    rand_grouping <- pracma::randperm(population_size)
    ops           <- 16

    for (p in seq(from = ops, to = population_size, by = ops)) {

      routes <- pop_rte[rand_grouping[(p - ops + 1):p], ]
      brks   <- pop_brk[rand_grouping[(p - ops + 1):p]]
      dists  <- total_dist[rand_grouping[(p - ops + 1):p]]
      ignore <- min(dists)
      idx    <- which.min(dists)

      best_of_8_rte <- routes[idx, ]
      best_of_8_brk <- brks[[idx]]
      rte_ins_pts   <- sort(ceiling(n * runif(n = 2, min = 0, max = 1)))
      if(length(unique(rte_ins_pts)) == 1){
        rte_ins_pts[1] <- rte_ins_pts[2] - 1
      }

      I <- rte_ins_pts[1]
      J <- rte_ins_pts[2]


      for(k in 1:ops){
        tmp_pop_rte[k, ]  <- best_of_8_rte
        tmp_pop_brk[[k]]  <- best_of_8_brk
        bst_len           <- length(best_of_8_brk)

        # Flip
        if(k == 2){
          tmp_pop_rte[k, I:J] <- matlab::fliplr(tmp_pop_rte[k, I:J])

          # Swap
        } else if(k == 3){
          tmp_pop_rte[k, c(I, J)] <- tmp_pop_rte[k, c(J, I)]

        } else if(k == 4){
          # Slide
          tmp_pop_rte[k, I:J] <- tmp_pop_rte[k, c((I + 1):J, I)]

        } else if(k == 5){
          # Change Breaks
          tmp_pop_brk[[k]]  <- RandomBreaks(num_agents = num_agents,
                                            n = n,
                                            min_tour = min_tour,
                                            max_tour_visits = max_tour_visits)

        } else if(k == 6){
          # Flip, Change Breaks
          tmp_pop_rte[k, I:J] <- matlab::fliplr(tmp_pop_rte[k, I:J])
          tmp_pop_brk[[k]]    <- RandomBreaks(num_agents = num_agents,
                                              n = n,
                                              min_tour = min_tour,
                                              max_tour_visits = max_tour_visits)


        } else if(k == 7){
          # Swap, Change Breaks
          tmp_pop_rte[k, c(I, J)] <- tmp_pop_rte[k, c(J, I)]
          tmp_pop_brk[[k]]        <- RandomBreaks(num_agents = num_agents,
                                                  n = n,
                                                  min_tour = min_tour,
                                                  max_tour_visits = max_tour_visits)


        } else if(k == 8){
          # Slide, Change Breaks
          tmp_pop_rte[k, I:J] <- tmp_pop_rte[k, c((I + 1):J, I)];
          tmp_pop_brk[[k]]    <- RandomBreaks(num_agents = num_agents,
                                              n = n,
                                              min_tour = min_tour,
                                              max_tour_visits = max_tour_visits)


        } else if(k == 9){
          l <- extraDistr::rdunif(n = 1, min = 1, max = min(n - J - 1, floor(sqrt(n))))
          if(is.nan(l) | is.na(l)){
            l <- 0
          }

          temp1 <- tmp_pop_rte[k, I:(I + l)]
          temp2 <- tmp_pop_rte[k, J:(J + l)]
          tmp_pop_rte[k, I:(I + l)] <- temp2[1:length(tmp_pop_rte[k, I:(I + l)])]
          tmp_pop_rte[k, I:(I + l)] <- temp1[1:length(tmp_pop_rte[k, I:(I + l)])]

        } else if(k == 12){
            #Remove tasks from agent
            l    <- extraDistr::rdunif(n = 1, min = 1, max = num_agents - 1)
            temp <- tmp_pop_brk[[k]]
            temp <- c(temp[1:(l - 1)], temp[(l + 1):length(temp)], n)
            tmp_pop_brk[[k]] <- temp[!is.na(temp)][1:bst_len]

        } else if(k == 13){
            l    <- extraDistr::rdunif(n = 1, min = 1, max = num_agents - 1)
            temp <- tmp_pop_brk[[k]]
            temp <- c(1, temp[1:(l - 1)], temp[(l + 1):length(temp)])
            tmp_pop_brk[[k]] <- temp[!is.na(temp)][1:bst_len]

        } else {
          if(I < n){
            tmp_pop_rte[k, c(I, I + 1)] <- tmp_pop_rte[k, c(I + 1, I) ]
          }
        }
      }

      new_pop_rte[(p - ops + 1):p, ]  <- tmp_pop_rte
      new_pop_brk[(p - ops + 1):p]    <- tmp_pop_brk

    }

    pop_rte <- new_pop_rte
    pop_brk <- new_pop_brk

  }


  best_tour <- matrix(data = NA, nrow = num_agents, ncol = n)
  for (i in 1:num_agents) {

    if(range[i, 1] <= range[i, 2]){
      best_tour[i, 1:(range[i, 2] - range[i, 1] + 3)] <- c(i, opt_route[c(range[i, 1] : range[i, 2])], i)
    } else{
      best_tour[i, 1:2] <- c(i, i)
    }
  }

  rownames(best_tour) <- paste0('Vehicle_', 1:nrow(best_tour))
  colnames(best_tour) <- paste0('Visit_', 0:(ncol(best_tour) - 1))
  first_empty_column  <- which(apply(best_tour, MARGIN = 2, FUN = function(i){all(is.na(i))}) == TRUE)[[1]]
  best_tour           <- best_tour[, 1:first_empty_column]

  # Return the routes ordering and the minimum distance estimated from the optimization procedure.
  # Since there is a penalty for routes exceeding the max_tour_distnace, we need to scale down the distances by this factor.
  min_dist     = ifelse(min_dist > max_tour_distance, min_dist / penalty_factor, min_dist)
  tour_lengths = ifelse(tour_lengths > max_tour_distance, tour_lengths / penalty_factor, tour_lengths)


  list(routes = best_tour,
       max_distance = min_dist,
       dist_history = as.numeric(dist_history),
       tour_lengths = tour_lengths)
}

