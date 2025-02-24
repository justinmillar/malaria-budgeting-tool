

create_plan_cost_summary <- function(data){

  #-SUMMARY------------------------------------------------------------------------------
  # Print a summary of the interventions and number of states/LGAs being targeted
  cat("Costing scenario being generated for the following mix of interventions:")
  print(
    data |>
      select(adm1, adm2, starts_with("code_")) |>
      pivot_longer(
        cols = starts_with("code_"),
        names_to = "intervention",
        names_prefix = "code_",
        values_to = "included"
      ) |>
      filter(included == 1) |>
      group_by(intervention) |>
      summarise(states_targeted = n_distinct(adm1),
                lgas_targeted = n_distinct(paste(adm1, adm2, sep = "_"))
      )
  )
  cat(data$plan_description[1])


  #-Add target population data------------------------------------------------------------
  target_population <-
    readxl::read_xlsx(
      "data/nga-demo-data-pre-processed/data-needs-not-user-defined.xlsx",
      sheet = "population"
    )

  #-Add landmass data---------------------------------------------------------------------
  landmass <-
    readxl::read_xlsx(
      "data/nga-demo-data-pre-processed/data-needs-not-user-defined.xlsx",
      sheet = "spatial-structure"
    )


  #-Add unit cost data---------------------------------------------------------------------
  unit_cost <-
    read.csv("data/nga-demo-data-pre-processed/uploads/unit_cost_data.csv")

  #-Function to get unit costs for calulations--------------------------------------------
  get_unit_cost <- function(resource_name, cost_column) {
    unit_cost %>%
      filter(resource == resource_name) %>%
      pull({{ cost_column }})
  }

  #-Generate quantifications---------------------------------------------------------------

  #-ITN CAMPAIGNS--------------------------------
  ## Assumptions - net quant is
  ## pop / 1.8 and  that 50
  ## nets make up a bale
  itn_campaign_quantification <-
    data |>
    select(
      adm1, adm2, contains("itn_campaign"),
      plan_shortname, plan_description
    ) |>
    filter(
      code_itn_campaign == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_total
        )
    ) |>
    mutate(
      quant_itn_campaign_nets = pop_total / 1.8
    ) |>
    mutate(
      quant_itn_campaign_bales = quant_itn_campaign_nets / 50
    ) |>
    rename(
      target_pop_itn_campaign = pop_total
    )

  # Compute costs in both NGN and USD
  itn_campaign_quantification <-
    itn_campaign_quantification  |>
    mutate(
      itn_campaign_net_procurement_cost_NGN = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Procurement per ITN (Dual AI)", ngn_cost),
      itn_campaign_net_distribution_cost_NGN = quant_itn_campaign_bales * get_unit_cost("ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs", ngn_cost),
      itn_campaign_campaign_cost_NGN = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Operational cost per ITN", ngn_cost),

      itn_campaign_net_procurement_cost_USD = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Procurement per ITN (Dual AI)", usd_cost),
      itn_campaign_net_distribution_cost_USD = quant_itn_campaign_bales * get_unit_cost("ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs", usd_cost),
      itn_campaign_campaign_cost_USD = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Operational cost per ITN", usd_cost),

      itn_campaign_total_cost_NGN = itn_campaign_net_procurement_cost_NGN + itn_campaign_net_distribution_cost_NGN + itn_campaign_campaign_cost_NGN,
      itn_campaign_total_cost_USD = itn_campaign_net_procurement_cost_USD + itn_campaign_net_distribution_cost_USD + itn_campaign_campaign_cost_USD
    )

  # Reshape data to include currency column
  itn_campaign_quantification <-
    itn_campaign_quantification |>
    pivot_longer(
      cols = starts_with("itn_campaign_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )  |>
    ungroup()

  #-ITN ROUTINE-------------------------------
  ## Assumptions - nets needed are
  ## 30% of pw and u5 pop
  itn_routine_quantifications <-
    data |>
    select(
      adm1, adm2, contains("itn_routine"),
      plan_shortname, plan_description
    ) |>
    filter(
      code_itn_routine == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_pw, pop_0_5)
    ) |>
    mutate(
      target_pop_itn_routine = pop_pw + pop_0_5
    ) |>
    mutate(
      quant_itn_routine_nets = target_pop_itn_routine * 0.3
    ) |>
    select(-pop_pw) |>
    select(-pop_0_5)

  # Compute costs in both NGN and USD
  itn_routine_quantifications <-
    itn_routine_quantifications  |>
    mutate(
      itn_routine_net_procurement_cost_NGN = quant_itn_routine_nets * get_unit_cost("ITN Routine Distribution-Pocurement cost per ITN (Dual AI)", ngn_cost),
      itn_routine_net_operational_cost_NGN = quant_itn_routine_nets * get_unit_cost("ITN Routine Distribution-Operational cost per ITN", ngn_cost),

      itn_routine_net_procurement_cost_USD = quant_itn_routine_nets * get_unit_cost("ITN Routine Distribution-Pocurement cost per ITN (Dual AI)", usd_cost),
      itn_routine_net_operational_cost_USD = quant_itn_routine_nets * get_unit_cost("ITN Routine Distribution-Operational cost per ITN", usd_cost),

      itn_routine_total_cost_NGN = itn_routine_net_procurement_cost_NGN + itn_routine_net_operational_cost_NGN,
      itn_routine_total_cost_USD = itn_routine_net_procurement_cost_USD + itn_routine_net_operational_cost_USD
    )

  # Reshape data to include currency column
  itn_routine_quantifications <-
    itn_routine_quantifications |>
    pivot_longer(
      cols = starts_with("itn_routine_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )


  #-IPTp-------------------------------------
  ## Assumptions - three doses
  ## of SP (in blister packs of 3 pills)
  ## targeted ANC attendence at 80%
  ## coverage with a 10% buffer stock
  iptp_quantifications <-
    data |>
    select(
      adm1, adm2, contains("iptp"),
      plan_shortname, plan_description
    ) |>
    filter(
      code_iptp == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_pw
        )
    ) |>
    mutate(
      quant_iptp_sp_doses = pop_pw * 0.8 * 3 * (1.1)
    ) |>
    rename(
      target_pop_iptp = pop_pw
    )

  # Compute costs in both NGN and USD
  iptp_quantifications <-
    iptp_quantifications  |>
    mutate(
      iptp_sp_procurement_cost_NGN = quant_iptp_sp_doses * get_unit_cost("IPTp-SP-Procurement cost per SP", ngn_cost),
      iptp_sp_distribution_cost_NGN = quant_iptp_sp_doses * get_unit_cost("IPTp-SP-Routine Distribution cost", ngn_cost),

      iptp_sp_procurement_cost_USD = quant_iptp_sp_doses * get_unit_cost("IPTp-SP-Procurement cost per SP", usd_cost),
      iptp_sp_distribution_cost_USD = quant_iptp_sp_doses * get_unit_cost("IPTp-SP-Routine Distribution cost", usd_cost),

      iptp_total_cost_NGN = iptp_sp_procurement_cost_NGN + iptp_sp_distribution_cost_NGN,
      iptp_total_cost_USD = iptp_sp_procurement_cost_USD + iptp_sp_distribution_cost_USD
    )

  # Reshape data to include currency column
  iptp_quantifications <-
    iptp_quantifications |>
    pivot_longer(
      cols = starts_with("iptp_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )


  #-SMC-----------------------------------------
  ## Since SMC is delivered in two dosage groups:
  ## 3 to <12 months and >12 to 59 months,
  ## the number of packets for each age group needs to be
  ##  quantified and then multiplied by 4, to cover every
  ##  cycle.  In addition, a buffer stock between
  ##  10 -20% should be included to accommodate for loss,
  ##  re-dosing and treatment of children from neighbouring locations.
  ##  Calculation:
  ##    A. Total number of children under 5.
  ##    B. Number of children 3 to <12 months = (18% of A)
  ##    C. 10% buffer stock for children 3 to <12 months = (10% of B)
  ##    D. Total number of packets for children 3 to <12 months needed for one cycle of SMC = (B + C)
  ##    E. Total number of packets for children 3 to <12 months needed for one round of SMC = (4 x D)
  ##    F. Number of children >12 to 59 months = (77% of A)
  ##    G. 10% buffer stock for children >12 to 59 months = (10% of F)
  ##    H. Total number of packets for children >12 to 59 months needed for one cycle of SMC = (F + G)
  ##    I. Total number of packets for children >12 to 59 months needed for one round of SMC = (4 x H)
  smc_monthly_rounds <- 4    #smc given over 4 months
  smc_pop_prop_3_11 <- 0.18  # 18% of the under 5 population is 3-11 months
  smc_pop_prop_12_59 <- 0.77 # 77% of the under 5 population is 12-59 months
  buffer = 1.1 # includes 10% buffer

  smc_quantification <-
    data |>
    select(
      adm1, adm2, contains("smc"),
      plan_shortname, plan_description
    ) |>
    filter(
      code_smc == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_0_5
        )
    ) |>
    mutate(
      quant_smc_spaq_3_11_months = pop_0_5 *  smc_pop_prop_3_11 * smc_monthly_rounds * buffer,
      quant_smc_spaq_12_59_months = pop_0_5 *  smc_pop_prop_12_59 * smc_monthly_rounds * buffer,
      quant_smc_sqaq_total = quant_smc_spaq_3_11_months + quant_smc_spaq_12_59_months,
      target_pop_smc = pop_0_5 * (smc_pop_prop_3_11 + smc_pop_prop_12_59)
    ) |>
    select(
      -pop_0_5
    )

  # Compute costs in both NGN and USD
  smc_quantification <-
    smc_quantification  |>
    mutate(
      smc_spaq_3_11_months_procurement_cost_NGN = quant_smc_spaq_3_11_months * get_unit_cost("SMC-SPAQ-3-11 months-Procurement cost per SPAQ", ngn_cost),
      smc_spaq_12_59_months_procurement_cost_NGN = quant_smc_spaq_12_59_months * get_unit_cost("SMC-SPAQ-12-59 months-Procurement cost per SPAQ", ngn_cost),
      smc_spaq_total_procurement_cost_NGN = smc_spaq_3_11_months_procurement_cost_NGN + smc_spaq_12_59_months_procurement_cost_NGN,
      smc_campaign_cost_NGN = target_pop_smc * get_unit_cost("SMC-Campaign cost per child", ngn_cost),


      smc_spaq_3_11_months_procurement_cost_USD = quant_smc_spaq_3_11_months * get_unit_cost("SMC-SPAQ-3-11 months-Procurement cost per SPAQ", usd_cost),
      smc_spaq_12_59_months_procurement_cost_USD = quant_smc_spaq_12_59_months * get_unit_cost("SMC-SPAQ-12-59 months-Procurement cost per SPAQ", usd_cost),
      smc_spaq_total_procurement_cost_USD = smc_spaq_3_11_months_procurement_cost_USD + smc_spaq_12_59_months_procurement_cost_USD,
      smc_campaign_cost_USD = target_pop_smc * get_unit_cost("SMC-Campaign cost per child", usd_cost),

      smc_total_cost_NGN = smc_spaq_total_procurement_cost_NGN + smc_campaign_cost_NGN,
      smc_total_cost_USD = smc_spaq_total_procurement_cost_USD + smc_campaign_cost_USD
    )

  # Reshape data to include currency column
  smc_quantification <-
    smc_quantification |>
    pivot_longer(
      cols = starts_with("smc_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )

  #-PMC-----------------------------------------------------------
  ## Assumptions
  ## Antigen coverage rate = 85% (since immunization is being
  ## used as the contact point).
  ## children 0-1 take 1 tab
  ## children 1-2 take 2 tab
  ## Since one in four children/infants in Nigeria is underweight),
  ## 25% of children <1 year will take half instead of one tablet,
  ##  while 25% of children 1-2 years will take one instead of 2 tablets.
  ##  A factor of 0.75% was therefore used to quantify the required SP
  ##  for each age group.
  ##  There will be 4 touch points within a calendar year for PMC
  ##  With a 10% buffer added
  pmc_quantification <-
    data |>
    select(
      adm1, adm2, contains("pmc"),
      plan_shortname, plan_description
    ) |>
    filter(
      code_pmc == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_0_1, pop_1_2
        )
    ) |>
    mutate(
      quant_pmc_sp_0_1_years = pop_0_1 * 0.85 * 4 * 0.75 * 1.1,
      quant_pmc_sp_1_2_years = pop_1_2 * 0.85 * 4 * 2 * 0.75 * 1.1,
      quant_pmc_total = quant_pmc_sp_0_1_years + quant_pmc_sp_1_2_years,
      target_pop_pmc =  pop_0_1 * 0.85 + pop_1_2 * 0.85
    ) |>
    select(-pop_0_1, -pop_1_2)

  # Compute costs in both NGN and USD
  pmc_quantification <-
    pmc_quantification  |>
    mutate(
      pmc_sp_procurement_cost_NGN = quant_pmc_total * get_unit_cost("PMC-SP-Procurement cost", ngn_cost),
      pmc_sp_distribution_cost_NGN = quant_pmc_total * get_unit_cost("PMC-SP-Routine Distribution cost", ngn_cost),
      pmc_operational_cost_NGN = target_pop_pmc * get_unit_cost("PMC-Cost per child per annum", ngn_cost),

      pmc_sp_procurement_cost_USD = quant_pmc_total * get_unit_cost("PMC-SP-Procurement cost", usd_cost),
      pmc_sp_distribution_cost_USD = quant_pmc_total * get_unit_cost("PMC-SP-Routine Distribution cost", usd_cost),
      pmc_operational_cost_USD = target_pop_pmc * get_unit_cost("PMC-Cost per child per annum", usd_cost),

      pmc_total_cost_NGN = pmc_sp_procurement_cost_NGN + pmc_sp_distribution_cost_NGN + pmc_operational_cost_NGN,
      pmc_total_cost_USD = pmc_sp_procurement_cost_USD + pmc_sp_distribution_cost_USD + pmc_operational_cost_USD
    )

  # Reshape data to include currency column
  pmc_quantification <-
    pmc_quantification |>
    pivot_longer(
      cols = starts_with("pmc_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )

  #-Vaccine----------------------------------------------------
  ## Assumptions
  ## 84% coverage
  ## 7% wasatge
  ## 4 doses per child
  vacc_quantification <-
    data |>
    select(
      adm1, adm2, contains("vacc"),
      plan_shortname, plan_description
    ) |>
    filter(
      code_vacc == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_vaccine_5_36_months
        )
    ) |>
    mutate(
      quant_vacc_doses = pop_vaccine_5_36_months * 0.84 * 1.07 * 4
    ) |>
    rename(
      target_pop_vacc = pop_vaccine_5_36_months
    )

  # Compute costs in both NGN and USD
  vacc_quantification <-
    vacc_quantification  |>
    mutate(
      vacc_procurement_cost_NGN = quant_vacc_doses * get_unit_cost("Malaria Vaccine-Procurement cost", ngn_cost),
      vacc_operational_cost_NGN = quant_vacc_doses * get_unit_cost("Malaria Vaccine-Operational cost", ngn_cost),

      vacc_procurement_cost_USD = quant_vacc_doses * get_unit_cost("Malaria Vaccine-Procurement cost", usd_cost),
      vacc_operational_cost_USD = quant_vacc_doses * get_unit_cost("Malaria Vaccine-Operational cost", usd_cost),

      vacc_total_cost_NGN = vacc_procurement_cost_NGN + vacc_operational_cost_NGN,
      vacc_total_cost_USD = vacc_procurement_cost_USD + vacc_operational_cost_USD
    )

  # Reshape data to include currency column
  vacc_quantification <-
    vacc_quantification |>
    pivot_longer(
      cols = starts_with("vacc_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )

  #-IRS------------------------------------------------------------
  ## This wasn't quantified previously
  ## but assumed that the cost of IRS
  ## is 10% of the ITN campaing cost * 5
  ## ???
  irs_quantification <-
    data |>
    select(
      adm1, adm2, contains("irs"),
      plan_shortname, plan_description
    ) |>
    filter(
      code_irs == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_total
        )
    ) |>
    mutate(
      quant_itn_campaign_nets = pop_total / 1.8
    ) |>
    mutate(
      quant_itn_campaign_bales = quant_itn_campaign_nets / 50
    ) |>
    rename(
      target_pop_itn_campaign = pop_total
    )

  # Compute costs in both NGN and USD
  irs_quantification <-
    irs_quantification  |>
    mutate(
      itn_campaign_net_procurement_cost_NGN = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Procurement per ITN (Dual AI)", ngn_cost),
      itn_campaign_net_distribution_cost_NGN = quant_itn_campaign_bales * get_unit_cost("ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs", ngn_cost),
      itn_campaign_campaign_cost_NGN = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Operational cost per ITN", ngn_cost),

      itn_campaign_net_procurement_cost_USD = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Procurement per ITN (Dual AI)", usd_cost),
      itn_campaign_net_distribution_cost_USD = quant_itn_campaign_bales * get_unit_cost("ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs", usd_cost),
      itn_campaign_campaign_cost_USD = quant_itn_campaign_nets * get_unit_cost("ITN Campaign-Operational cost per ITN", usd_cost),

      itn_campaign_total_cost_NGN = itn_campaign_net_procurement_cost_NGN + itn_campaign_net_distribution_cost_NGN + itn_campaign_campaign_cost_NGN,
      itn_campaign_total_cost_USD = itn_campaign_net_procurement_cost_USD + itn_campaign_net_distribution_cost_USD + itn_campaign_campaign_cost_USD
    ) |>
    mutate(
      irs_campaign_total_cost_NGN = itn_campaign_total_cost_NGN * 0.1 * 5,
      irs_campaign_total_cost_USD = itn_campaign_total_cost_USD * 0.1 * 5
    ) |>
    select(-contains("itn"))

  # Reshape data to include currency column
  irs_quantification <-
    irs_quantification |>
    pivot_longer(
      cols = starts_with("irs_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )

  #-LSM-------------------------------------------------------
  ## Assumptions
  ## Based on LGA landmass assuming a 2% coverage
  ## of landmass with LGA
  ## Bti chemical is quantified by taking
  ## Ha landcoverage (2% of landmass * 100) * 0.5 kg per HA * 24  times sprayed
  lsm_quantification <-
    data |>
    select(
      adm0, adm1, adm2, contains("lsm"),
      plan_shortname, plan_description
    ) |>
    left_join(landmass) |>
    mutate(
      quant_lsm_bti = (landmass_sq_km * 0.002 * 100) * 0.5 * 24
    )

  # Compute costs in both NGN and USD
  lsm_quantification <-
    lsm_quantification  |>
    mutate(
      lsm_procurement_cost_NGN = quant_lsm_bti * get_unit_cost("LSM-Bti-Procurement cost per kg", ngn_cost),
      lsm_operational_cost_NGN = 1 * get_unit_cost("LSM-Bti-Procurement cost per kg", ngn_cost),

      lsm_procurement_cost_USD = quant_lsm_bti * get_unit_cost("LSM-Bti-Procurement cost per kg", usd_cost),
      lsm_operational_cost_USD = 1 * get_unit_cost("LSM-Bti-Procurement cost per kg", usd_cost),

      lsm_total_cost_NGN = lsm_procurement_cost_NGN + lsm_operational_cost_NGN,
      lsm_total_cost_USD = lsm_procurement_cost_USD + lsm_operational_cost_USD
    )

  # Reshape data to include currency column
  lsm_quantification <-
    lsm_quantification |>
    pivot_longer(
      cols = starts_with("lsm_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )


  #-CASE MANAGEMENT-----------------------------------------------------------------
  case_management_quantification <-
    read.csv(
      "data/nga-demo-data-pre-processed/cm-quant-data.csv"
    )

  # Compute costs in both NGN and USD
  case_management_quantification <-
    case_management_quantification  |>
    mutate(
      cm_rdt_kit_procurement_cost_NGN = cm_rdt_kit_quantity * get_unit_cost("Case Management-RDT kits-Procurement cost per kit & consumables", ngn_cost),
      cm_rdt_kit_distribution_cost_NGN = cm_rdt_kit_quantity * get_unit_cost("Case Management-RDT kits-Distribution cost per kit & consumables", ngn_cost),
      cm_act_packs_procurement_cost_NGN = cm_act_packs_quantity * get_unit_cost("Case Management-AL-Procurement cost per AL", ngn_cost),
      cm_act_packs_distribution_cost_NGN = cm_act_packs_quantity * get_unit_cost("Case Management-AL-Routine Distribution cost per AL", ngn_cost),
      cm_iv_artesunate_procurement_cost_NGN = cm_iv_artesunate_quantity * get_unit_cost("Case Management-Artesunate injections-Procurement cost", ngn_cost),
      cm_iv_artesunate_distribution_cost_NGN = cm_iv_artesunate_quantity * get_unit_cost("Case Management-Artesunate injections-Routine Distribution cost", ngn_cost),
      cm_ras_procurement_cost_NGN = cm_ras_quantity * get_unit_cost("Case Management-Rectal Artesunate Suppositories (RAS)-Procurement cost per RAS", ngn_cost),
      cm_ras_distribution_cost_NGN = cm_ras_quantity * get_unit_cost("Case Management-RAS-Routine Distribution cost per RAS", ngn_cost),

      cm_rdt_kit_procurement_cost_USD = cm_rdt_kit_quantity * get_unit_cost("Case Management-RDT kits-Procurement cost per kit & consumables", usd_cost),
      cm_rdt_kit_distribution_cost_USD = cm_rdt_kit_quantity * get_unit_cost("Case Management-RDT kits-Distribution cost per kit & consumables", usd_cost),
      cm_act_packs_procurement_cost_USD = cm_act_packs_quantity * get_unit_cost("Case Management-AL-Procurement cost per AL", usd_cost),
      cm_act_packs_distribution_cost_USD = cm_act_packs_quantity * get_unit_cost("Case Management-AL-Routine Distribution cost per AL", usd_cost),
      cm_iv_artesunate_procurement_cost_USD = cm_iv_artesunate_quantity * get_unit_cost("Case Management-Artesunate injections-Procurement cost", usd_cost),
      cm_iv_artesunate_distribution_cost_USD = cm_iv_artesunate_quantity * get_unit_cost("Case Management-Artesunate injections-Routine Distribution cost", usd_cost),
      cm_ras_procurement_cost_USD = cm_ras_quantity * get_unit_cost("Case Management-Rectal Artesunate Suppositories (RAS)-Procurement cost per RAS", usd_cost),
      cm_ras_distribution_cost_USD = cm_ras_quantity * get_unit_cost("Case Management-RAS-Routine Distribution cost per RAS", usd_cost),

    ) |>
    mutate(
      cm_public_total_cost_NGN = cm_rdt_kit_procurement_cost_NGN + cm_rdt_kit_distribution_cost_NGN +
        cm_act_packs_procurement_cost_NGN + cm_act_packs_distribution_cost_NGN + cm_iv_artesunate_procurement_cost_NGN +
        cm_iv_artesunate_distribution_cost_NGN + cm_ras_procurement_cost_NGN + cm_ras_distribution_cost_NGN,
      cm_public_total_cost_USD =  cm_rdt_kit_procurement_cost_USD + cm_rdt_kit_distribution_cost_USD +
        cm_act_packs_procurement_cost_USD + cm_act_packs_distribution_cost_USD + cm_iv_artesunate_procurement_cost_USD +
        cm_iv_artesunate_distribution_cost_USD + cm_ras_procurement_cost_USD + cm_ras_distribution_cost_USD
    ) |>
    mutate(
      cm_private_NGN = cm_public_total_cost_NGN/0.423*0.577,
      cm_private_USD = cm_public_total_cost_USD/0.423*0.577
    )

  # adjust for different scenarios
  if(data$plan_shortname[1] == "Plan B"){
    case_management_quantification$cm_private_NGN  <- case_management_quantification$cm_private_NGN * 0.2
    case_management_quantification$cm_private_USD  <- case_management_quantification$cm_private_USD * 0.2
  }

  if(data$plan_shortname[1] == "Plan C"){
    case_management_quantification$cm_private_NGN <- case_management_quantification$cm_private_NGN * 0.1
    case_management_quantification$cm_private_USD <- case_management_quantification$cm_private_USD * 0.1
  }

  if(data$plan_shortname[1] == "Plan D"){
    case_management_quantification$cm_private_NGN  <- 0
    case_management_quantification$cm_private_USD  <- 0
  }

  # Reshape data to include currency column
  case_management_quantification <-
    case_management_quantification |>
    pivot_longer(
      cols = starts_with("cm_"),
      names_to = "cost_type",
      values_to = "cost"
    )  |>
    mutate(
      currency = ifelse(grepl("_NGN", cost_type), "NGN", "USD"),
      cost_type = gsub("_NGN|_USD", "", cost_type)
    ) |>
    pivot_wider(
      names_from = "cost_type", values_from = "cost"
    )


  #-LGA INTERVENTION MIX DATA FRAME---------------------------------------------

  # Ensure all datasets have the same column type for `currency`
  datasets <- list(
    itn_campaign_quantification,
    itn_routine_quantifications,
    iptp_quantifications,
    smc_quantification,
    pmc_quantification,
    vacc_quantification,
    irs_quantification,
    lsm_quantification,
    case_management_quantification
  )

  # Standardize the 'currency' column type across all datasets
  datasets <- lapply(datasets, function(df) {
    if ("currency" %in% colnames(df)) {
      df <- df |> mutate(currency = as.character(currency))
    }
    return(df)
  })

  # Combine all datasets into one
  lga_mix <-
    bind_rows(
      datasets
    ) |>
    crossing(year = 2025:2027) |>
    fill(plan_shortname, .direction = "downup") |>
    fill(plan_description, .direction = "downup") |>
    fill(adm0, .direction = "downup") |>
    select(-adm3) |>
    group_by( adm0,
              adm1,
              adm2,
              plan_shortname,
              plan_description,
              year,
              currency
    ) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    left_join(target_population |> select(-adm0, -adm3, -year)) |>
    # ADD TOTAL BUDGET COL
    mutate(
      total_budget = rowSums(across(contains("total_cost")), na.rm = TRUE),
      total_budget_per_person = total_budget / pop_total
    )

  #-STATE INTERVENTION MIX DATA FRAME-----------------------------------------------
  state_mix <-
    lga_mix |>
    select(-total_budget, -total_budget_per_person) |>
    group_by(
      adm0,
      adm1,
      plan_shortname,
      plan_description,
      year,
      currency
    ) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    # ADD TOTAL BUDGET COL
    mutate(
      total_budget = rowSums(across(contains("total_cost")), na.rm = TRUE),
      total_budget_per_person = total_budget / pop_total
    )

  #-NATIONAL INTERVENTION MIX DATA FRAME--------------------------------------------
  national_mix <-
    lga_mix |>
    select(-total_budget, -total_budget_per_person) |>
    group_by(
      adm0,
      plan_shortname,
      plan_description,
      year,
      currency
    ) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    mutate(
      cm_eqa_cost = case_when(
        currency == "NGN" ~ 134033351,
        currency == "USD" ~ 83770.84
        ),
      itn_campaign_storage_hardware_cost = case_when(
        currency == "NGN" ~ 8000000,
        currency == "USD" ~ 5000
      ),
      ento_surveillance_total_cost = case_when(
        currency == "NGN" ~  270769231,
        currency == "USD" ~  169230.77
      )
    ) |>
    mutate(
      cm_public_total_cost = cm_public_total_cost + cm_eqa_cost,
      itn_campaign_total_cost = itn_campaign_total_cost + itn_campaign_storage_hardware_cost
    )


    #-ADD SUPPORT SERVICES COSTS-------------------------------------------------------
    # at the national level for ease of working demo
    national_support_services <-
      read.csv("data/nga-demo-data-pre-processed/codable-national-data-october.csv") |>
      select(
        gf_wd_malaria_long_haul_distribution:rm_total_cost
      ) |>
      crossing(currency = c("NGN", "USD")) |>
      mutate(across(
         where(is.numeric),  # Apply only to numeric columns
        ~ ifelse(currency == "USD", . / 1600, .)
      ))

   national_mix <-
     national_mix |>
     left_join(national_support_services, by = "currency") |>
    mutate(
      total_budget = rowSums(across(contains("total_cost")), na.rm = TRUE),
      total_budget_per_person = total_budget / pop_total
    )


    #-MAKE intervention mix map------------------------------------------------------
   mix_map <-
     data |>
     crossing(year = c(2025, 2026, 2027)) |>
     select(-code_itn_urban) |>
     # Pivot the data from wide to long format
     pivot_longer(cols = starts_with("code_"),
                  names_to = "intervention",
                  values_to = "value")  |>
     select(-starts_with("type_")) |>
     # Filter to keep only rows where the intervention is set to 1
     filter(value == 1)  |>
     mutate(intervention = str_remove(intervention, "code_")) |>
     # create a new column for nicely coded intervention names
     mutate(
       intervention_mix_to_show = str_to_upper(intervention)) |>
     mutate(
       intervention_mix_to_show = case_when(
         intervention == "cm_public" ~ "CM",
         intervention == "iptp" ~ "IPTp",
         intervention == "vacc" ~ "Vaccine",
         intervention == "itn_routine" ~ "ITN Routine",

         intervention == "itn_campaign" ~ "ITN Campaign",

         TRUE ~ intervention_mix_to_show)
     ) |>
     group_by(adm1, adm2, plan_shortname, plan_description, year)  |>
     # Concatenate interventions with "+" separator
     mutate(intervention_summary = paste(intervention_mix_to_show, collapse = " + ")) |>
     # remove case management private from the mix
     mutate(intervention_summary =str_remove_all(intervention_summary, "\\s*\\+ CM_PRIVATE$")) |>
     mutate(intervention_summary =str_remove_all(intervention_summary, "CM_PRIVATE\\s*\\+\\s*")) |>
     # reduce down
     select(-intervention, -value, -intervention_mix_to_show) |>
     distinct()

   mix_shp <-
    sf::st_read("data/nga-demo-data-pre-processed/shapefiles/lga_shapefile_simp.shp") |>
     left_join(mix_map, by = c("state" = "adm1",
                               "lga" = "adm2"))

   static_plot_mix_map <-
     data |>
     crossing(year = c(2025, 2026, 2027)) |>
     select(-code_itn_urban) |>
     pivot_longer(
       cols = starts_with("code_"),  # Selects all 'code_*' columns
       names_to = "intervention",  # New column for intervention names
       values_to = "code"  # Values from 'code_*' columns
     ) %>%
     filter(code == 1) %>%  # Keep only rows where code == 1
     mutate(
       intervention_type = case_when(
         intervention == "code_cm_public" ~ NA,
         intervention == "code_iptp" ~ type_iptp,
         intervention == "code_smc" ~ type_smc,
         intervention == "code_pmc" ~ type_pmc,
         intervention == "code_vacc" ~ type_vacc,
         intervention == "code_irs" ~ type_irs,
         intervention == "code_itn_campaign" ~ type_itn_campaign,
         intervention == "code_itn_routine" ~ type_itn_routine,
         intervention == "code_lsm" ~ type_lsm,
         TRUE ~ NA_character_  # Assigns NA if no matching type column exists
       )
     ) %>%
     mutate(intervention = str_remove(intervention, "code_")) |>
     # create a new column for nicely coded intervention names
     mutate(
       intervention = case_when(
         intervention == "cm_public" ~ "Case Management Public",
         intervention == "cm_private" ~ "Case Management Private",
         intervention == "iptp" ~ "IPTp",
         intervention == "vacc" ~ "Vaccine",
         intervention == "itn_routine" ~ "ITN Routine",
         intervention == "itn_campaign" ~ "ITN Campaign",
         intervention == "smc" ~ "SMC",
         intervention == "pmc" ~ "PMC",
         intervention == "irs" ~ "IRS",
         intervention == "lsm" ~ "LSM",
         TRUE ~ intervention)
     ) |>
     select(adm0, adm1, adm2, plan_shortname, plan_description, intervention, intervention_type, year)

   static_shp <-
     sf::st_read("data/nga-demo-data-pre-processed/shapefiles/lga_shapefile_simp.shp") |>
     left_join(static_plot_mix_map, by = c("state" = "adm1",
                               "lga" = "adm2"))


   #-FORMAT AND SAVE DATA------------------------------------------------------------

   # save intervention mix map
   sf::st_write(mix_shp,
                paste0("data/nga-demo-data-pre-processed/shapefiles/",
                       data$plan_shortname[1],
                       "-interactive-map.shp"),
                delete_dsn = TRUE)  # Overwrites existing file

   sf::st_write(static_shp,
                paste0("data/nga-demo-data-pre-processed/shapefiles/",
                       data$plan_shortname[1],
                       "-static-map.shp"),
                delete_dsn = TRUE)  # Overwrites existing file

   # save raw budgets into single sheet

   # Create a new workbook
   wb <- openxlsx::createWorkbook()

   # Add each dataframe as a sheet
   openxlsx::addWorksheet(wb, "LGA")
   openxlsx::writeData(wb, "LGA", lga_mix)

   openxlsx::addWorksheet(wb, "State")
   openxlsx::writeData(wb, "State", state_mix)

   openxlsx::addWorksheet(wb, "National")
   openxlsx::writeData(wb, "National", national_mix)

   # Save workbook
   openxlsx::saveWorkbook(wb, paste0("data/nga-demo-data-pre-processed/budgets-generated/"
                           ,data$plan_shortname[1], "-budgets.xlsx"), overwrite = TRUE)


}


