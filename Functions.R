
##------------------------------------------------------------------------------
##--                 Get previous data
##------------------------------------------------------------------------------

get_p_data <- function(proposal_id, p_sales_report_id, personnel_assessment_id) {
  
  ## p_sales ----
  db_sales_report <- mongo(collection = "SalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
  sales_report_info <- db_sales_report$find(query = paste0('{"_id": {"$oid": "', p_sales_report_id, '"}}'))
  hospital_sales_report_ids <- sales_report_info$`hospital-sales-report-ids`[[1]]
  
  db_hospital_sales_report <- mongo(collection = "HospitalSalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
  p_hospital_sales_report_info <- data.frame()
  for (i in hospital_sales_report_ids) {
    info <- db_hospital_sales_report$find(query = paste0('{"_id": {"$oid": "', i, '"}}'))
    p_hospital_sales_report_info <- bind_rows(p_hospital_sales_report_info, info)
  }
  
  ## product ----
  goods_config_id <- p_hospital_sales_report_info$`goods-config-id`[!duplicated(p_hospital_sales_report_info$`goods-config-id`)]
  
  db_goods <- mongo(collection = "GoodsConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  goods_info <- db_goods$find(query = paste0('{"_id": {"$oid": "', goods_config_id, '"}}'), fields = '{}')
  
  db_product <- mongo(collection = "ProductConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  product_info <- db_product$find(query = paste0('{"_id": {"$oid": "', goods_info$`goods-id`, '"}}'), fields = '{}')
  
  product <- goods_info %>% 
    left_join(product_info, by = c("goods-id" = "_id")) %>% 
    select(`_id`, `product-id`, `life-cycle`)
  
  ## hospital ----
  dest_config_ids <- p_hospital_sales_report_info$`dest-config-id`
  
  db_dest <- mongo(collection = "DestConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  dest_info <- data.frame()
  for (i in dest_config_ids) {
    info <- db_dest$find(query = paste0('{"_id": {"$oid": "', i, '"}}'), fields = '{}')
    dest_info <- bind_rows(dest_info, info)
  }
  
  db_hospital <- mongo(collection = "HospitalConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  dest_ids <- dest_info$`dest-id`
  hospital_info <- data.frame()
  for (i in dest_ids) {
    info <- db_hospital$find(query = paste0('{"_id": {"$oid": "', i, '"}}'), fields = '{}')
    hospital_info <- bind_rows(hospital_info, info)
  }
  
  hospital <- dest_info %>% 
    left_join(hospital_info, by = c("dest-id" = "_id")) %>% 
    select(`_id`, `hospital-id`)
  
  ## p_intermedia ----
  db_intermedia <- mongo(collection = "Intermedia", db = options()$mongodb$db, url = options()$mongodb$host)
  p_intermedia_info <- db_intermedia$find(query = paste0('{"proposal-id": "', proposal_id, '"}'))
  p_intermedia <- p_intermedia_info$initial_phase[[1]]
  
  p_hospital_sales_info <- p_hospital_sales_report_info %>% 
    left_join(product, by = c("goods-config-id" = "_id")) %>% 
    left_join(hospital, by = c("dest-config-id" = "_id")) %>% 
    left_join(p_intermedia, by = c("hospital-id" = "hosp_id")) %>% 
    select(`hospital-id`, `hosp_size`, `product-id`, `life-cycle`, 
           `sales`, `share`, `offer_attractiveness`, `customer_relationship`, `potential`)
  colnames(p_hospital_sales_info) <- c("hosp_id", "hosp_size", "prod_id", "life_cycle", 
                                       "p_sales", "p_market_share", "p_offer_attractiveness", "p_customer_relationship", "p_potential")
  
  ## p_rep ----
  db_personnel_assessment <- mongo(collection = "PersonnelAssessment", db = options()$mongodb$db, url = options()$mongodb$host)
  personnel_assessment_info <- db_personnel_assessment$find(query = paste0('{"_id": {"$oid": "', personnel_assessment_id, '"}}'))
  rep_ability_ids <- personnel_assessment_info$`representative-ability-ids`[[1]]
  
  db_rep_ability <- mongo(collection = "RepresentativeAbility", db = options()$mongodb$db, url = options()$mongodb$host)
  p_rep_ability_info <- data.frame()
  for (i in rep_ability_ids) {
    info <- db_rep_ability$find(query = paste0('{"_id": {"$oid": "', i, '"}}'))
    p_rep_ability_info <- bind_rows(p_rep_ability_info, info)
  }
  
  p_rep_ability_info1 <- p_rep_ability_info %>% 
    select(`representative-id`, `product-knowledge`, `sales-ability`, 
           `regional-management-ability`, `job-enthusiasm`, `behavior-validity`)
  colnames(p_rep_ability_info1) <- c("rep_id", "p_product_knowledge", "p_sales_skills", "p_territory_management_ability", 
                                     "p_work_motivation", "p_behavior_efficiency")
  ## output ----
  output <- list(p_hospital_sales_info = p_hospital_sales_info, 
                 p_rep_ability_info = p_rep_ability_info1)
  
  return(output)
}

##------------------------------------------------------------------------------
##--                 Get input data
##------------------------------------------------------------------------------

get_input_data <- function(input_id) {
  
  ## paper_input ----
  db_input <- mongo(collection = "Paperinput", db = options()$mongodb$db, url = options()$mongodb$host)
  input_info <- db_input$find(query = paste0('{"_id": {"$oid": "', input_id, '"}}'))
  business_input_ids <- input_info$`business-input-ids`[[1]]
  rep_input_ids <- input_info$`representative-input-ids`[[1]]
  manager_input_id <- input_info$`manager-input-ids`[[1]]
  
  ## total_budget ----
  db_scenario <- mongo(collection = "Scenario", db = options()$mongodb$db, url = options()$mongodb$host)
  scenario_info <- db_scenario$find(query = paste0('{"proposal-id": "', proposal_id, '", "phase": ', format(phase, nsmall = 1), '}'), fields = '{}')
  scenario_id <- scenario_info$`_id`
  
  db_resource <- mongo(collection = "ResourceConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  resource_info <- db_resource$find(query = paste0('{"scenario-id": "', scenario_id, '"}'), fields = '{}')
  manager_config_id <- resource_info$`resource-id`[which(resource_info$`resource-type` == 0)]
  
  db_manager <- mongo(collection = "ManagerConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  manager_info <- db_manager$find(query = paste0('{"_id": {"$oid": "', manager_config_id, '"}}'))
  total_budget <- manager_info$`total-budgets`
  
  ## business_input ----
  db_business_input <- mongo(collection = "Businessinput", db = options()$mongodb$db, url = options()$mongodb$host)
  business_input_info <- data.frame()
  for (i in business_input_ids) {
    info <- db_business_input$find(query = paste0('{"_id": {"$oid": "', i, '"}}'))
    business_input_info <- bind_rows(business_input_info, info)
  }
  resource_config_ids <- business_input_info$`resource-config-id`[!duplicated(business_input_info$`resource-config-id`)]
  goods_config_id <- business_input_info$`goods-config-id`[!duplicated(business_input_info$`goods-config-id`)]
  dest_config_ids <- business_input_info$`dest-config-id`[!duplicated(business_input_info$`dest-config-id`)]
  
  # representative
  resource_info <- resource_info %>% 
    filter(`resource-type` == 1)
  
  db_rep <- mongo(collection = "RepresentativeConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  resource_ids <- resource_info$`resource-id`
  rep_info <- data.frame()
  for (i in resource_ids) {
    info <- db_rep$find(query = paste0('{"_id": {"$oid": "', i, '"}}'), fields = '{}')
    rep_info <- bind_rows(rep_info, info)
  }
  
  representative <- resource_info %>% 
    left_join(rep_info, by = c("resource-id" = "_id")) %>% 
    select(`_id`, `representative-id`)
  
  # product
  db_goods <- mongo(collection = "GoodsConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  goods_info <- db_goods$find(query = paste0('{"_id": {"$oid": "', goods_config_id, '"}}'), fields = '{}')
  
  db_product <- mongo(collection = "ProductConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  product_info <- db_product$find(query = paste0('{"_id": {"$oid": "', goods_info$`goods-id`, '"}}'), fields = '{}')
  
  product <- goods_info %>% 
    left_join(product_info, by = c("goods-id" = "_id")) %>% 
    select(`_id`, `product-id`)
  
  # hospital
  db_dest <- mongo(collection = "DestConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  dest_info <- data.frame()
  for (i in dest_config_ids) {
    info <- db_dest$find(query = paste0('{"_id": {"$oid": "', i, '"}}'), fields = '{}')
    dest_info <- bind_rows(dest_info, info)
  }
  
  db_hospital <- mongo(collection = "HospitalConfig", db = options()$mongodb$db, url = options()$mongodb$host)
  dest_ids <- dest_info$`dest-id`
  hospital_info <- data.frame()
  for (i in dest_ids) {
    info <- db_hospital$find(query = paste0('{"_id": {"$oid": "', i, '"}}'), fields = '{}')
    hospital_info <- bind_rows(hospital_info, info)
  }
  
  hospital <- dest_info %>% 
    left_join(hospital_info, by = c("dest-id" = "_id")) %>% 
    select(`_id`, `hospital-id`)
  
  business_input <- business_input_info %>% 
    left_join(hospital, by = c("dest-config-id" = "_id")) %>% 
    left_join(resource_info, by = c("resource-config-id" = "_id")) %>% 
    left_join(rep_info, by = c("resource-id" = "_id")) %>% 
    left_join(product, by = c("goods-config-id" = "_id")) %>% 
    mutate(total_budget = total_budget) %>% 
    select(`dest-config-id`, `hospital-id`, `resource-config-id`, `representative-id`, `goods-config-id`, `product-id`, 
           `sales-target`, `budget`, `meeting-places`, `visit-time`, `total_budget`)
  colnames(business_input) <- c("dest_id", "hosp_id", "resource_id", "rep_id", "goods_id", "prod_id", 
                                "quota", "budget", "meeting_attendance", "call_time_factor", "total_budget")
  
  ## rep_input ----
  db_rep_input <- mongo(collection = "Representativeinput", db = options()$mongodb$db, url = options()$mongodb$host)
  rep_input_info <- data.frame()
  for (i in rep_input_ids) {
    info <- db_rep_input$find(query = paste0('{"_id": {"$oid": "', i, '"}}'))
    rep_input_info <- bind_rows(rep_input_info, info)
  }
  
  rep_input <- resource_info %>% 
    left_join(rep_info, by = c("resource-id" = "_id")) %>% 
    select(`_id`, `representative-id`) %>% 
    left_join(rep_input_info, by = c("_id" = "resource-config-id")) %>% 
    select(`_id`, `representative-id`, `product-knowledge-training`, `sales-ability-training`, `region-training`, 
           `performance-training`, `vocational-development`, `ability-coach`, `assist-access-time`)
  colnames(rep_input) <- c("resource_id", "rep_id", "product_knowledge_training", "sales_skills_training", "territory_management_training", 
                           "performance_review", "career_development_guide", "one_on_one_coaching", "field_work")
  
  ## manager_input ----
  db_manager_input <- mongo(collection = "Managerinput", db = options()$mongodb$db, url = options()$mongodb$host)
  manager_input_info <- db_manager_input$find(query = paste0('{"_id": {"$oid": "', manager_input_id, '"}}'))
  
  manager_input <- select(manager_input_info, 
                          `strategy-analysis-time`, `admin-work-time`, `client-management-time`, `kpi-analysis-time`, `team-meeting-time`)
  colnames(manager_input) <- c("business_strategy_planning", "admin_work", "kol_management", "employee_kpi_and_compliance_check", "team_meeting")
  
  ## output ----
  output <- list(business_input = business_input, 
                 rep_input = rep_input, 
                 manager_input = manager_input)
  
  return(output)
}

##------------------------------------------------------------------------------
##--                 Generate data to use
##------------------------------------------------------------------------------

get_data2use <- function(p_data, input_data) {
  
  output <- input_data$business_input %>% 
    left_join(input_data$rep_input, by = c("resource_id", "rep_id")) %>% 
    bind_cols(input_data$manager_input[rep(1, each = 10), ]) %>% 
    left_join(p_data$p_hospital_sales_info, by = c("hosp_id", "prod_id")) %>% 
    left_join(p_data$p_rep_ability_info, by = c("rep_id")) %>% 
    select(`dest_id`, `hosp_id`, `hosp_size`, `p_sales`, `p_market_share`, `p_offer_attractiveness`, `p_customer_relationship`, `p_potential`, 
           `resource_id`, `rep_id`, `p_territory_management_ability`, `p_sales_skills`, `p_product_knowledge`, `p_behavior_efficiency`, `p_work_motivation`, 
           `goods_id`, `prod_id`, `life_cycle`, `quota`, `budget`, `meeting_attendance`, `call_time_factor`, `total_budget`, 
           `field_work`, `one_on_one_coaching`, `team_meeting`, `business_strategy_planning`, `admin_work`, `employee_kpi_and_compliance_check`, `kol_management`, 
           `territory_management_training`, `sales_skills_training`, `product_knowledge_training`, `performance_review`, `career_development_guide`)
  
  return(output)
}

##------------------------------------------------------------------------------
##--                 Get curves and weightages
##------------------------------------------------------------------------------

get_intermedia <- function(uuid, type) {
  
  db_intermedia <- mongo(collection = "Intermedia", db = options()$mongodb$db, url = options()$mongodb$host)
  
  intermedia <- db_intermedia$find(query = paste0('{"uuid": "', uuid, '"}'), fields = paste0('{"_id": 0, "', type, '": 1}'))[[1]]
  intermedia <- as.list(intermedia)
  for (i in names(intermedia)) {
    intermedia[[i]] <- intermedia[[i]][[1]]
  }
  
  return(intermedia)
}

##------------------------------------------------------------------------------
##--                 Curves computation
##------------------------------------------------------------------------------

curve_func <- function(curve, curves, input) {
  
  curve_data <- curves[[curve]]
  
  if (input < min(curve_data$x))
    return(curve_data[which.min(curve_data$x), 2])
  
  if (input > max(curve_data$x))
    return(curve_data[which.max(curve_data$x), 2])
  
  left <- curve_data[which.min(abs(input - curve_data$x)), ]
  tmp <- curve_data[-which.min(abs(input - curve_data$x)), ]
  right <- tmp[which.min(abs(input - tmp$x)), ]
  
  y <- ifelse(left$x <= right$x,
              (1 - (input - left$x) / (right$x - left$x)) * left$y + (1 - (right$x - input) / (right$x - left$x)) * right$y, 
              (1 - (input - right$x) / (left$x - right$x)) * right$y + (1 - (left$x - input) / (left$x - right$x)) * left$y)
  
  return(y)
}

##------------------------------------------------------------------------------
##--                 Calculation function
##------------------------------------------------------------------------------

get_results <- function(dat, curves, weightages) {
  
  dat <- dat %>% 
    mutate(budget = budget / total_budget * 100)
  
  # general ability
  dat01 <- dat %>% 
    mutate(work_motivation = p_work_motivation + (10 - p_work_motivation) * 0.15 * (performance_review + career_development_guide),
           territory_management_ability = p_territory_management_ability + (10 - p_territory_management_ability) * 0.3 * territory_management_training,
           sales_skills = p_sales_skills + (10 - p_sales_skills) * 0.3 * sales_skills_training,
           product_knowledge = p_product_knowledge + (10 - p_product_knowledge) * 0.3 * product_knowledge_training,
           behavior_efficiency_factor = sapply(one_on_one_coaching, function(x) {curve_func("curve09", curves, x)}),
           behavior_efficiency = p_behavior_efficiency + (10 - p_behavior_efficiency) * behavior_efficiency_factor,
           general_ability = (territory_management_ability * weightages[["weightage02"]]$territory_management_ability + 
                                sales_skills * weightages[["weightage02"]]$sales_skills + 
                                product_knowledge * weightages[["weightage02"]]$product_knowledge + 
                                behavior_efficiency * weightages[["weightage02"]]$behavior_efficiency + 
                                work_motivation * weightages[["weightage02"]]$work_motivation) * 10)
  
  # rep ability efficiency
  dat02 <- dat01 %>% 
    mutate(quota_restriction_factor = ifelse(quota / p_sales < 0.5 | quota / p_sales > 2, 
                                             0.8, 
                                             ifelse(quota / p_sales >= 0.5 & quota / p_sales <= 2, 
                                                    1, 
                                                    0)),
           quota_restriction_factor = sapply(quota_restriction_factor, function(x) {curve_func("curve14", curves, x)}),
           rep_ability_efficiency = general_ability * weightages[["weightage03"]]$general_ability + 
             call_time_factor * weightages[["weightage03"]]$call_time_factor + 
             quota_restriction_factor * weightages[["weightage03"]]$quota_restriction_factor)
  
  # field work factor
  dat03 <- dat02 %>% 
    mutate(field_work_factor = sapply(field_work, function(x) {curve_func("curve16", curves, x)}))
  
  # deployment quality
  dat04 <- dat03 %>% 
    mutate(business_strategy_planning_factor = sapply(business_strategy_planning, function(x) {curve_func("curve18", curves, x)}),
           admin_work_factor = sapply(admin_work, function(x) {curve_func("curve19", curves, x)}),
           employee_kpi_and_compliance_check_factor = sapply(employee_kpi_and_compliance_check, function(x) {curve_func("curve20", curves, x)}),
           team_meeting_factor = sapply(team_meeting, function(x) {curve_func("curve21", curves, x)}),
           kol_management_factor = sapply(kol_management, function(x) {curve_func("curve22", curves, x)}),
           deployment_quality = business_strategy_planning_factor * weightages[["weightage04"]]$business_strategy_planning_factor + 
             admin_work_factor * weightages[["weightage04"]]$admin_work_factor + 
             employee_kpi_and_compliance_check_factor * weightages[["weightage04"]]$employee_kpi_and_compliance_check_factor + 
             team_meeting_factor * weightages[["weightage04"]]$team_meeting_factor + 
             kol_management_factor * weightages[["weightage04"]]$kol_management_factor)
  
  # sales performance
  dat05 <- dat04 %>% 
    mutate(sales_performance = rep_ability_efficiency * weightages[["weightage05"]]$rep_ability_efficiency + 
             field_work_factor * weightages[["weightage05"]]$field_work_factor + 
             deployment_quality * weightages[["weightage05"]]$deployment_quality)
  
  # customer relationship
  dat06 <- dat05 %>% 
    mutate(budget_factor = ifelse(hosp_size == 1, 
                                  sapply(budget, function(x) {curve_func("curve02", curves, x)}), 
                                  ifelse(hosp_size == 2, 
                                         sapply(budget, function(x) {curve_func("curve03", curves, x)}), 
                                         ifelse(hosp_size == 3, 
                                                sapply(budget, function(x) {curve_func("curve04", curves, x)}), 
                                                0))),
           meeting_attendance_factor = ifelse(hosp_size == 1, 
                                              sapply(meeting_attendance, function(x) {curve_func("curve05", curves, x)}), 
                                              ifelse(hosp_size == 2, 
                                                     sapply(meeting_attendance, function(x) {curve_func("curve06", curves, x)}), 
                                                     ifelse(hosp_size == 3, 
                                                            sapply(meeting_attendance, function(x) {curve_func("curve07", curves, x)}), 
                                                            0))),
           customer_relationship_factor = budget_factor * weightages[["weightage06"]]$budget_factor + 
             meeting_attendance_factor * weightages[["weightage06"]]$meeting_attendance_factor,
           customer_relationship = p_customer_relationship + (100 - p_customer_relationship) * customer_relationship_factor)
  
  # current oa
  dat07 <- dat06 %>% 
    mutate(current_oa = sales_performance * weightages[["weightage07"]]$sales_performance + 
             customer_relationship * weightages[["weightage07"]]$customer_relationship)
  
  # offer attractiveness
  dat08 <- dat07 %>% 
    mutate(offer_attractiveness = ifelse(life_cycle == "导入期", 
                                         current_oa * weightages[["weightage10"]]$current_oa + 
                                           p_offer_attractiveness * weightages[["weightage10"]]$p_offer_attractiveness, 
                                         ifelse(life_cycle == "成熟期", 
                                                current_oa * weightages[["weightage11"]]$current_oa + 
                                                  p_offer_attractiveness * weightages[["weightage11"]]$p_offer_attractiveness, 
                                                0)))
  
  # market share, sales
  dat09 <- dat08 %>% 
    mutate(potential = p_potential,
           market_share = sapply(offer_attractiveness, function(x) {curve_func("curve28", curves, x)}),
           market_share = round(market_share / 100, 2),
           sales = round(potential * market_share / 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)))
  
  return(dat09)
}

##------------------------------------------------------------------------------
##--                 Update representative information
##------------------------------------------------------------------------------

get_rep_ability <- function(results) {
  
  rep_ability <- results %>% 
    group_by(rep_id, product_knowledge, sales_skills, territory_management_ability, work_motivation, behavior_efficiency) %>% 
    summarise(potential = sum(potential),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(work_motivation = ifelse(sales / quota >= 0.9 & sales / quota <= 1.2, 
                                    work_motivation + (10 - work_motivation) * 0.2, 
                                    ifelse(sales / quota < 0.9 | sales / quota > 1.2, 
                                           work_motivation, 
                                           0))) %>%
    mutate(product_knowledge = round(product_knowledge, 1),
           sales_skills = round(sales_skills, 1),
           territory_management_ability = round(territory_management_ability, 1),
           work_motivation = round(work_motivation, 1),
           behavior_efficiency = round(behavior_efficiency, 1)) %>% 
    select(`rep_id`, `product_knowledge`, `sales_skills`, `territory_management_ability`, `work_motivation`, `behavior_efficiency`)
  colnames(rep_ability) <- c("representative-id", "product-knowledge", "sales-ability", "regional-management-ability", "job-enthusiasm", "behavior-validity")
  
  return(rep_ability)
}

get_action_kpi <- function(p_action_kpi, rep_ability) {
  
  action_kpi <- p_action_kpi %>% 
    left_join(rep_ability, by = c("representative-id")) %>% 
    mutate(class1 = ifelse(`behavior-validity` >= 0 & `behavior-validity` < 3, 
                           1, 
                           ifelse(`behavior-validity` >= 3 & `behavior-validity` < 6, 
                                  2, 
                                  ifelse(`behavior-validity` >= 6 & `behavior-validity` < 8, 
                                         3, 
                                         ifelse(`behavior-validity` >= 8 & `behavior-validity` <= 10, 
                                                4, 
                                                0))))) %>% 
    mutate(`target-coverage` = ifelse(class1 == 1, 
                                      sapply(`target-coverage`, function(x) {x - sample(5:10, 1)}), 
                                      ifelse(class1 == 2, 
                                             sapply(`target-coverage`, function(x) {x - sample(0:5, 1)}), 
                                             ifelse(class1 == 3, 
                                                    sapply(`target-coverage`, function(x) {x + sample(0:5, 1)}), 
                                                    ifelse(class1 == 4, 
                                                           sapply(`target-coverage`, function(x) {x + sample(5:10, 1)}), 
                                                           0))))) %>% 
    mutate(class2 = ifelse(`job-enthusiasm` >= 0 & `job-enthusiasm` < 3, 
                           1, 
                           ifelse(`job-enthusiasm` >= 3 & `job-enthusiasm`< 6, 
                                  2, 
                                  ifelse(`job-enthusiasm` >= 6 & `job-enthusiasm` < 8, 
                                         3, 
                                         ifelse(`job-enthusiasm` >= 8 & `job-enthusiasm` < 10, 
                                                4, 
                                                0))))) %>% 
    mutate(`high-level-frequency` =  ifelse(class1 == 1, 
                                            sapply(`high-level-frequency`, function(x) {sample(13:14, 1)}), 
                                            ifelse(class1 == 2, 
                                                   sapply(`high-level-frequency`, function(x) {sample(14:15, 1)}), 
                                                   ifelse(class1 == 3, 
                                                          sapply(`high-level-frequency`, function(x) {sample(16:18, 1)}), 
                                                          ifelse(class1 == 4, 
                                                                 sapply(`high-level-frequency`, function(x) {sample(19:22, 1)}), 
                                                                 0)))),
           `middle-level-frequency` = ifelse(class1 == 1, 
                                             sapply(`middle-level-frequency`, function(x) {sample(13:14, 1)}), 
                                             ifelse(class1 == 2, 
                                                    sapply(`middle-level-frequency`, function(x) {sample(13:14, 1)}), 
                                                    ifelse(class1 == 3, 
                                                           sapply(`middle-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                           ifelse(class1 == 4, 
                                                                  sapply(`middle-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                                  0)))),
           `low-level-frequency` = ifelse(class1 == 1, 
                                          sapply(`low-level-frequency`, function(x) {sample(13:14, 1)}), 
                                          ifelse(class1 == 2, 
                                                 sapply(`low-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                 ifelse(class1 == 3, 
                                                        sapply(`low-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                        ifelse(class1 == 4, 
                                                               sapply(`low-level-frequency`, function(x) {sample(11:12, 1)}), 
                                                               0))))) %>% 
    mutate(`high-level-frequency` = ifelse(class2 == 1, 
                                           sapply(`high-level-frequency`, function(x) {x - sample(1:2, 1)}), 
                                           ifelse(class2 == 2, 
                                                  sapply(`high-level-frequency`, function(x) {x - sample(0:1, 1)}), 
                                                  ifelse(class2 == 3, 
                                                         sapply(`high-level-frequency`, function(x) {x + sample(0:1, 1)}),
                                                         ifelse(class2 == 4, 
                                                                `high-level-frequency` + 1,
                                                                0)))),
           `middle-level-frequency` = ifelse(class2 == 1, 
                                             `middle-level-frequency` - 2, 
                                             ifelse(class2 == 2, 
                                                    `middle-level-frequency` - 1, 
                                                    ifelse(class2 == 3, 
                                                           sapply(`middle-level-frequency`, function(x) {x + sample(0:1, 1)}), 
                                                           ifelse(class2 == 4, 
                                                                  `middle-level-frequency` + 1, 
                                                                  0)))),
           `low-level-frequency` = ifelse(class2 == 1, 
                                          `low-level-frequency` - 2, 
                                          ifelse(class2 == 2, 
                                                 `low-level-frequency` - 1, 
                                                 ifelse(class2 == 3, 
                                                        sapply(`low-level-frequency`, function(x) {x + sample(0:1, 1)}), 
                                                        ifelse(class2 == 4, 
                                                               `low-level-frequency` + 1, 
                                                               0))))) %>% 
    select(`representative-id`, `target-number`, `target-coverage`, `high-level-frequency`, `middle-level-frequency`, `low-level-frequency`)
  
  return(action_kpi)
}

##------------------------------------------------------------------------------
##--                 Generate reports
##------------------------------------------------------------------------------

get_hosp_report <- function(results) {
  
  hosp_report <- results %>% 
    mutate(growth = round(sales / p_sales - 1, 2)) %>% 
    select(`dest_id`, `resource_id`, `goods_id`, `potential`, `sales`, `quota`, `market_share`, `quota_rate`, `growth`)
  colnames(hosp_report) <- c("dest-config-id", "resource-config-id", "goods-config-id", "potential", "sales", "sales-quota", "share", "quota-achievement", "sales-growth")
  
  return(hosp_report)
}

get_rep_report <- function(results) {
  
  rep_report <- results %>% 
    select(`resource_id`, `goods_id`, `potential`, `p_sales`, `sales`, `quota`) %>% 
    group_by(resource_id, goods_id) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(market_share = round(sales / potential * 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_sales - 1, 2)) %>% 
    select(`resource_id`, `goods_id`, `potential`, `sales`, `quota`, `market_share`, `quota_rate`, `growth`)
  colnames(rep_report) <- c("resource-config-id", "goods-config-id", "potential", "sales", "sales-quota", "share", "quota-achievement", "sales-growth")
  
  return(rep_report)
}

get_prod_report <- function(results, p_sales_report_id) {
  
  prod1_report <- results %>% 
    select(`goods_id`, `potential`, `p_sales`, `sales`, `quota`) %>% 
    group_by(goods_id) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(market_share = round(sales / potential * 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_sales - 1, 2)) %>% 
    select(`goods_id`, `sales`, `quota`, `market_share`, `quota_rate`, `growth`)
  
  db_sales_report <- mongo(collection = "SalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
  sales_report_info <- db_sales_report$find(query = paste0('{"_id": {"$oid": "', p_sales_report_id, '"}}'))
  product_sales_report_ids <- head(sales_report_info$`product-sales-report-ids`[[1]], 3)
  
  db_product_sales_report <- mongo(collection = "ProductSalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
  p_product_sales_report_info <- data.frame()
  for (i in product_sales_report_ids) {
    info <- db_product_sales_report$find(query = paste0('{"_id": {"$oid": "', i, '"}}'), fields = '{}')
    p_product_sales_report_info <- bind_rows(p_product_sales_report_info, info)
  }
  p_product_sales_report_info <- arrange(p_product_sales_report_info, `goods-config-id`)
  
  market_share1 <- sample(50:55, 1)/100 - prod1_report$market_share
  market_share2 <- market_share1 * sample(60:75, 1)/100
  market_share3 <- market_share1 - market_share2
  
  potential <- prod1_report$sales / prod1_report$market_share
  
  prod2_report <- tibble(goods_id = p_product_sales_report_info$`goods-config-id`[2:3],
                         market_share = c(market_share2, market_share3)) %>% 
    mutate(sales = round(potential * market_share, 2),
           quota = round(sales, -5),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_product_sales_report_info$sales[2:3] - 1, 2),
           market_share = round(market_share, 2))
  
  prod_report <- bind_rows(prod1_report, prod2_report)
  
  colnames(prod_report) <- c("goods-config-id", "sales", "sales-quota", "share", "quota-achievement", "sales-growth")
  
  return(prod_report)
}





