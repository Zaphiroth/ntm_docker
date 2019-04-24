
##------------------------------------------------------------------------------
##--                 Get previous data
##------------------------------------------------------------------------------

get_p_data <- function(paper_info) {
  
  proposal_id <- paper_info$`proposal-id`
  sales_report_ids <- paper_info$`sales-report-ids`[[1]]
  personnel_assessment_ids <- paper_info$`personnel-assessment-ids`[[1]]
  
  ## p_sales ----
  db_sales_report <- mongo(collection = "SalesReport", db = "pharbers-ntm-client", url = options()$mongodb$host)
  sales_report_info <- db_sales_report$find(query = paste0('{"_id" : {"$oid" : "', sales_report_ids[length(sales_report_ids)], '"}}'))
  hospital_sales_report_ids <- sales_report_info$`hospital-sales-report-ids`[[1]]
  representative_sales_report_ids <- sales_report_info$`representative-sales-report-ids`[[1]]
  product_sales_report_ids <- sales_report_info$`product-sales-report-ids`[[1]]
  
  db_hospital_sales_report <- mongo(collection = "HospitalSalesReport", db = "pharbers-ntm-client", url = options()$mongodb$host)
  p_hospital_sales_report_info <- data.frame()
  for (i in 1:length(hospital_sales_report_ids)) {
    info <- db_hospital_sales_report$find(query = paste0('{"_id" : {"$oid" : "', hospital_sales_report_ids[i], '"}}'))
    p_hospital_sales_report_info <- bind_rows(p_hospital_sales_report_info, info)
  }
  
  ## product ----
  goods_config_id <- p_hospital_sales_report_info$`goods-config-id`[!duplicated(p_hospital_sales_report_info$`goods-config-id`)]
  
  db_goods <- mongo(collection = "GoodsConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  goods_info <- db_goods$find(query = paste0('{"_id" : {"$oid" : "', goods_config_id, '"}}'), fields = '{}')
  
  db_product <- mongo(collection = "ProductConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  product_info <- db_product$find(query = paste0('{"_id" : {"$oid" : "', goods_info$`goods-id`, '"}}'), fields = '{}')
  
  product <- goods_info %>% 
    left_join(product_info, by = c("goods-id" = "_id")) %>% 
    select(`_id`, `product-id`, `life-cycle`)
  
  ## hospital ----
  dest_config_ids <- p_hospital_sales_report_info$`dest-config-id`
  
  db_dest <- mongo(collection = "DestConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  dest_info <- data.frame()
  for (i in 1:length(dest_config_ids)) {
    info <- db_dest$find(query = paste0('{"_id" : {"$oid" : "', dest_config_ids[i], '"}}'), fields = '{}')
    dest_info <- bind_rows(dest_info, info)
  }
  
  db_hospital <- mongo(collection = "HospitalConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  hospital_info <- data.frame()
  for (i in 1:length(dest_info$`dest-id`)) {
    info <- db_hospital$find(query = paste0('{"_id" : {"$oid" : "', dest_info$`dest-id`[i], '"}}'), fields = '{}')
    hospital_info <- bind_rows(hospital_info, info)
  }
  
  hospital <- dest_info %>% 
    left_join(hospital_info, by = c("dest-id" = "_id")) %>% 
    select(`_id`, `hospital-id`)
  
  ## p_intermedia ----
  db_intermedia <- mongo(collection = "Intermedia", db = "pharbers-ntm-client", url = options()$mongodb$host)
  p_intermedia_info <- db_intermedia$find(query = paste0('{"proposal-id" : "', proposal_id, '"}'))
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
  db_personnel_assessment <- mongo(collection = "PersonnelAssessment", db = "pharbers-ntm-client", url = options()$mongodb$host)
  personnel_assessment_info <- db_personnel_assessment$find(query = paste0('{"_id" : {"$oid" : "', personnel_assessment_ids[length(personnel_assessment_ids)], '"}}'))
  rep_ability_ids <- personnel_assessment_info$`representative-ability-ids`[[1]]
  
  db_rep_ability <- mongo(collection = "RepresentativeAbility", db = "pharbers-ntm-client", url = options()$mongodb$host)
  p_rep_ability_info <- data.frame()
  for (i in 1:length(rep_ability_ids)) {
    info <- db_rep_ability$find(query = paste0('{"_id" : {"$oid" : "', rep_ability_ids[i], '"}}'))
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

get_input_data <- function(paper_info) {
  
  input_ids <- paper_info$`input-ids`[[1]]
  
  ## paper_input ----
  db_input <- mongo(collection = "Paperinput", db = "pharbers-ntm-client", url = options()$mongodb$host)
  input_info <- db_input$find(query = paste0('{"_id" : {"$oid" : "', input_ids[length(input_ids)], '"}}'))
  business_input_ids <- input_info$`business-input-ids`[[1]]
  rep_input_ids <- input_info$`representative-input-ids`[[1]]
  manager_input_id <- input_info$`manager-input-ids`[[1]]
  
  ## business_input ----
  db_business_input <- mongo(collection = "Businessinput", db = "pharbers-ntm-client", url = options()$mongodb$host)
  business_input_info <- data.frame()
  for (i in 1:length(business_input_ids)) {
    info <- db_business_input$find(query = paste0('{"_id" : {"$oid" : "', business_input_ids[i], '"}}'))
    business_input_info <- bind_rows(business_input_info, info)
  }
  resource_config_ids <- business_input_info$`resource-config-id`[!duplicated(business_input_info$`resource-config-id`)]
  goods_config_id <- business_input_info$`goods-config-id`[!duplicated(business_input_info$`goods-config-id`)]
  dest_config_ids <- business_input_info$`dest-config-id`[!duplicated(business_input_info$`dest-config-id`)]
  
  # representative
  db_resource <- mongo(collection = "ResourceConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  resource_info <- data.frame()
  for (i in 1:length(resource_config_ids)) {
    info <- db_resource$find(query = paste0('{"_id" : {"$oid" : "', resource_config_ids[i], '"}}'), fields = '{}')
    resource_info <- bind_rows(resource_info, info)
  }
  
  db_rep <- mongo(collection = "RepresentativeConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  rep_info <- data.frame()
  for (i in 1:length(resource_config_ids)) {
    info <- db_rep$find(query = paste0('{"_id" : {"$oid" : "', resource_info$`resource-id`[i], '"}}'), fields = '{}')
    rep_info <- bind_rows(rep_info, info)
  }
  
  representative <- resource_info %>% 
    left_join(rep_info, by = c("resource-id" = "_id")) %>% 
    select(`_id`, `representative-id`)
  
  # product
  db_goods <- mongo(collection = "GoodsConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  goods_info <- db_goods$find(query = paste0('{"_id" : {"$oid" : "', goods_config_id, '"}}'), fields = '{}')
  
  db_product <- mongo(collection = "ProductConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  product_info <- db_product$find(query = paste0('{"_id" : {"$oid" : "', goods_info$`goods-id`, '"}}'), fields = '{}')
  
  product <- goods_info %>% 
    left_join(product_info, by = c("goods-id" = "_id")) %>% 
    select(`_id`, `product-id`)
  
  # hospital
  db_dest <- mongo(collection = "DestConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  dest_info <- data.frame()
  for (i in 1:length(dest_config_ids)) {
    info <- db_dest$find(query = paste0('{"_id" : {"$oid" : "', dest_config_ids[i], '"}}'), fields = '{}')
    dest_info <- bind_rows(dest_info, info)
  }
  
  db_hospital <- mongo(collection = "HospitalConfig", db = "pharbers-ntm-client", url = options()$mongodb$host)
  hospital_info <- data.frame()
  for (i in 1:length(dest_info$`dest-id`)) {
    info <- db_hospital$find(query = paste0('{"_id" : {"$oid" : "', dest_info$`dest-id`[i], '"}}'), fields = '{}')
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
    select(`dest-config-id`, `hospital-id`, `resource-config-id`, `representative-id`, `goods-config-id`, `product-id`, 
           `sales-target`, `budget`, `meeting-places`, `visit-time`)
  colnames(business_input) <- c("dest_id", "hosp_id", "resource_id", "rep_id", "goods_id", "prod_id", 
                                "quota", "budget", "meeting_attendance", "call_time_factor")
  
  ## rep_input ----
  db_rep_input <- mongo(collection = "Representativeinput", db = "pharbers-ntm-client", url = options()$mongodb$host)
  rep_input_info <- data.frame()
  for (i in 1:length(rep_input_ids)) {
    info <- db_rep_input$find(query = paste0('{"_id" : {"$oid" : "', rep_input_ids[i], '"}}'))
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
  db_manager_input <- mongo(collection = "Managerinput", db = "pharbers-ntm-client", url = options()$mongodb$host)
  manager_input_info <- db_manager_input$find(query = paste0('{"_id" : {"$oid" : "', manager_input_id, '"}}'))
  
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
           `goods_id`, `prod_id`, `life_cycle`, `quota`, `budget`, `meeting_attendance`, `call_time_factor`, 
           `field_work`, `one_on_one_coaching`, `team_meeting`, `business_strategy_planning`, `admin_work`, `employee_kpi_and_compliance_check`, `kol_management`, 
           `territory_management_training`, `sales_skills_training`, `product_knowledge_training`, `performance_review`, `career_development_guide`)
  
  return(output)
}

##------------------------------------------------------------------------------
##--                 Get curves and weightages
##------------------------------------------------------------------------------

get_intermedia <- function(uuid, type) {
  
  # uuid <- "intermedia"
  # type <- "curves"
  # type <- "weightages"
  
  db_intermedia <- mongo(collection = "Intermedia", db = "pharbers-ntm-client", url = options()$mongodb$host)
  
  intermedia <- db_intermedia$find(query = '{"uuid" : "intermedia"}', fields = paste0('{"_id" : 0, "', type, '" : 1}'))[[1]]
  intermedia <- as.list(intermedia)
  for (i in names(intermedia)) {
    intermedia[[i]] <- intermedia[[i]][[1]]
  }
  
  return(intermedia)
}

##------------------------------------------------------------------------------
##--                 Curves computation
##------------------------------------------------------------------------------

curve_func <- function(curve, input) {
  
  # curve <- "curve09"
  # input <- 5
  # curves <- curves
  
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

get_results <- function(dat) {
  
  # dat <- dat
  
  # general ability
  dat01 <- dat %>% 
    mutate(work_motivation = p_work_motivation + (10 - p_work_motivation) * 0.15 * (performance_review + career_development_guide),
           territory_management_ability = p_territory_management_ability + (10 - p_territory_management_ability) * 0.3 * territory_management_training,
           sales_skills = p_sales_skills + (10 - p_sales_skills) * 0.3 * sales_skills_training,
           product_knowledge = p_product_knowledge + (10 - p_product_knowledge) * 0.3 * product_knowledge_training,
           behavior_efficiency_delta = mapply(curve_func, "curve09", one_on_one_coaching),
           behavior_efficiency = p_behavior_efficiency + behavior_efficiency_delta,
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
           quota_restriction_factor = mapply(curve_func, "curve14", quota_restriction_factor),
           rep_ability_efficiency = general_ability * weightages[["weightage03"]]$general_ability + 
             call_time_factor * weightages[["weightage03"]]$call_time_factor + 
             quota_restriction_factor * weightages[["weightage03"]]$quota_restriction_factor)
  
  # field work factor
  dat03 <- dat02 %>% 
    mutate(field_work_factor = mapply(curve_func, "curve16", field_work))
  
  # deployment quality
  dat04 <- dat03 %>% 
    mutate(business_strategy_planning_factor = mapply(curve_func, "curve18", business_strategy_planning),
           admin_work_factor = mapply(curve_func, "curve19", admin_work),
           employee_kpi_and_compliance_check_factor = mapply(curve_func, "curve20", employee_kpi_and_compliance_check),
           team_meeting_factor = mapply(curve_func, "curve21", team_meeting),
           kol_management_factor = mapply(curve_func, "curve22", kol_management),
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
                                  mapply(curve_func, "curve02", budget), 
                                  ifelse(hosp_size == 2, 
                                         mapply(curve_func, "curve03", budget), 
                                         ifelse(hosp_size == 3, 
                                                mapply(curve_func, "curve04", budget), 
                                                0))),
           meeting_attendance_factor = ifelse(hosp_size == 1, 
                                              mapply(curve_func, "curve05", meeting_attendance), 
                                              ifelse(hosp_size == 2, 
                                                     mapply(curve_func, "curve06", meeting_attendance), 
                                                     ifelse(hosp_size == 3, 
                                                            mapply(curve_func, "curve07", meeting_attendance), 
                                                            0))),
           customer_relationship_delta = budget_factor * weightages[["weightage06"]]$budget_factor + 
             meeting_attendance_factor * weightages[["weightage06"]]$meeting_attendance_factor,
           customer_relationship = p_customer_relationship - (10 - p_customer_relationship) * 0.1 + customer_relationship_delta)
  # current oa
  dat07 <- dat06 %>% 
    mutate(current_oa = sales_performance * weightages[["weightage07"]]$sales_performance + 
             customer_relationship * weightages[["weightage07"]]$customer_relationship)
  
  # offer attractiveness
  dat08 <- dat07 %>% 
    mutate(offer_attractiveness = ifelse(life_cycle == "生命周期1", # "growth", 
                                         current_oa * weightages[["weightage10"]]$current_oa + 
                                           p_offer_attractiveness * weightages[["weightage10"]]$p_offer_attractiveness, 
                                         ifelse(life_cycle == "maturity", 
                                                current_oa * weightages[["weightage11"]]$current_oa + 
                                                  p_offer_attractiveness * weightages[["weightage11"]]$p_offer_attractiveness, 
                                                0)))
  
  # market share, sales
  dat09 <- dat08 %>% 
    mutate(potential = p_potential * 1.05,
           market_share = mapply(curve_func, "curve28", offer_attractiveness),
           sales = potential * market_share / 100,
           quota_rate = sales / quota)
  
  return(dat09)
}

##------------------------------------------------------------------------------
##--                 Update representative information
##------------------------------------------------------------------------------

get_rep_ability <- function(results) {
  
  rep_ability <- results %>% 
    mutate(work_motivation = ifelse(sales/quota > 0.9 & sales/quota < 1.2, 
                                    work_motivation + (10 - work_motivation) * 0.3, 
                                    work_motivation)) %>% 
    select(`rep_id`, `product_knowledge`, `sales_skills`, `territory_management_ability`, `work_motivation`, `behavior_efficiency`) %>% 
    distinct()
  colnames(rep_ability) <- c("representative-id", "product-knowledge", "sales-ability", "regional-management-ability", "job-enthusiasm", "behavior-validity")
  
  return(rep_ability)
}

get_action_kpi <- function(p_action_kpi, rep_ability) {
  
  action_kpi <- p_action_kpi %>% 
    left_join(rep_ability, by = c("representative-id")) %>% 
    mutate(class1 = ifelse(`behavior-validity` >= 0 & `behavior-validity` <= 3, 
                           1, 
                           ifelse(`behavior-validity` > 3 & `behavior-validity` <= 6, 
                                  2, 
                                  ifelse(`behavior-validity` > 6 & `behavior-validity` <= 8, 
                                         3, 
                                         ifelse(`behavior-validity` > 8 & `behavior-validity` <= 10, 
                                                4, 
                                                0))))) %>% 
    mutate(`target-coverage` = ifelse(class1 == 1, 
                                      sample(25:35, 1), 
                                      ifelse(class1 == 2, 
                                             sample(35:45, 1), 
                                             ifelse(class1 == 3, 
                                                    sample(45:55, 1), 
                                                    ifelse(class1 == 4, 
                                                           sample(55:65, 1), 
                                                           0))))) %>% 
    mutate(class2 = ifelse(`job-enthusiasm` >= 0 & `job-enthusiasm` <= 3, 
                           1, 
                           ifelse(`job-enthusiasm` > 3 & `job-enthusiasm`<= 6, 
                                  2, 
                                  ifelse(`job-enthusiasm` > 6 & `job-enthusiasm` <= 8, 
                                         3, 
                                         ifelse(`job-enthusiasm` > 8 & `job-enthusiasm` <= 10, 
                                                4, 
                                                0))))) %>% 
    mutate(`high-level-frequency` =  ifelse(class1 == 1, 
                                            sample(10:12, 1), 
                                            ifelse(class1 == 2, 
                                                   sample(13:15, 1), 
                                                   ifelse(class1 == 3, 
                                                          sample(16:18, 1), 
                                                          ifelse(class1 == 4, 
                                                                 sample(19:21, 1), 
                                                                 0)))),
           `middle-level-frequency` = ifelse(class1 == 1, 
                                             sample(16:17, 1), 
                                             ifelse(class1 == 2, 
                                                    sample(15:16, 1), 
                                                    ifelse(class1 == 3, 
                                                           sample(13:14, 1), 
                                                           ifelse(class1 == 4, 
                                                                  sample(12:13), 
                                                                  0)))),
           `low-level-frequency` = ifelse(class1 == 1, 
                                          sample(16:17, 1), 
                                          ifelse(class1 == 2, 
                                                 sample(15:16, 1), 
                                                 ifelse(class1 == 3, 
                                                        sample(13:14, 1), 
                                                        ifelse(class1 == 4, 
                                                               sample(12:13), 
                                                               0))))) %>% 
    mutate(`high-level-frequency` = ifelse(class2 == 1, 
                                           `high-level-frequency` - sample(1:2, 1), 
                                           ifelse(class2 == 2, 
                                                  `high-level-frequency` - sample(0:1, 1), 
                                                  ifelse(class2 == 3, 
                                                         `high-level-frequency` + sample(0:1, 1), 
                                                         ifelse(class2 == 4, 
                                                                `high-level-frequency` + sample(1:2, 1), 
                                                                0)))),
           `middle-level-frequency` = ifelse(class2 == 1, 
                                             `high-level-frequency` - sample(1:2, 1), 
                                             ifelse(class2 == 2, 
                                                    `high-level-frequency` - sample(0:1, 1), 
                                                    ifelse(class2 == 3, 
                                                           `high-level-frequency` + sample(0:1, 1), 
                                                           ifelse(class2 == 4, 
                                                                  `high-level-frequency` + sample(1:2, 1), 
                                                                  0)))),
           `low-level-frequency` = ifelse(class2 == 1, 
                                          `high-level-frequency` - sample(1:2, 1), 
                                          ifelse(class2 == 2, 
                                                 `high-level-frequency` - sample(0:1, 1), 
                                                 ifelse(class2 == 3, 
                                                        `high-level-frequency` + sample(0:1, 1), 
                                                        ifelse(class2 == 4, 
                                                               `high-level-frequency` + sample(1:2, 1), 
                                                               0))))) %>% 
    select(`representative-id`, `target-number`, `target-coverage`, `high-level-frequency`, `middle-level-frequency`, `low-level-frequency`)
  
  return(action_kpi)
}

##------------------------------------------------------------------------------
##--                 Generate reports
##------------------------------------------------------------------------------

get_hosp_report <- function(results) {
  
  hosp_report <- results %>% 
    select(`dest_id`, `goods_id`, `potential`, `sales`, `quota`) %>% 
    group_by(dest_id, goods_id) %>% 
    summarise(potential = sum(potential),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    mutate(market_share = sales / potential * 100,
           quota_rate = sales / quota * 100) %>% 
    select(`dest_id`, `goods_id`, `potential`, `sales`, `quota`, `market_share`, `quota_rate`)
  colnames(hosp_report) <- c("dest-config-id", "goods-config-id", "potential", "sales", "sales-quota", "share", "quota-achievement")
  
  return(hosp_report)
}

get_rep_report <- function(results) {
  
  rep_report <- results %>% 
    select(`resource_id`, `goods_id`, `potential`, `sales`, `quota`) %>% 
    group_by(resource_id, goods_id) %>% 
    summarise(potential = sum(potential),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    mutate(market_share = sales / potential * 100,
           quota_rate = sales / quota * 100) %>% 
    select(`resource_id`, `goods_id`, `potential`, `sales`, `quota`, `market_share`, `quota_rate`)
  colnames(rep_report) <- c("resource-config-id", "goods-config-id", "potential", "sales", "sales-quota", "share", "quota-achievement")
  
  return(rep_report)
}

get_prod_report <- function(results) {
  
  prod_report <- results %>% 
    select(`goods_id`, `potential`, `sales`, `quota`) %>% 
    group_by(goods_id) %>% 
    summarise(potential = sum(potential),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    mutate(market_share = sales / potential * 100,
           quota_rate = sales / quota * 100) %>% 
    select(`goods_id`, `sales`, `quota`, `market_share`, `quota_rate`)
  colnames(prod_report) <- c("goods-config-id", "sales", "sales-quota", "share", "quota-achievement")
  
  return(prod_report)
}






