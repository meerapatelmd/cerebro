
create_join_key_col <-
  function(data) {

    data2 <- data
    colnames(data2) <- "join_key"

    cbind(data, data2)

  }

process_ingredients <-
  function(product,
           active_ingredients,
           inactive_ingredients) {
active_ingredients_processed <-
  factor(
    trimws(
      strsplit(active_ingredients,
               split = ",")[[1]],
      which = "both"
    ),
    levels =
      trimws(
        strsplit(active_ingredients,
                 split = ",")[[1]],
        which = "both"
      )
  )

inactive_ingredients_processed <-
  factor(
    trimws(
      strsplit(inactive_ingredients,
               split = ",")[[1]],
      which = "both"
    ),
    levels =
      trimws(
        strsplit(inactive_ingredients,
                 split = ",")[[1]],
        which = "both"
      )
    )

list(
  ACTIVE =
    tibble(!!product := active_ingredients_processed),
  INACTIVE =
    tibble(!!product := inactive_ingredients_processed)
)
}


product_1 <- "EltaMD UV Lotion Broad-Spectrum SPF 30 Plus (7 oz.)"
active_ingredients_1 <- "Octinoxate 7.5%, Zinc Oxide 7.0%"
inactive_ingredients_1 <- "Purified Water, Petrolatum, Isopropyl Palmitate, Octyl Stearate, Glyceryl Stearate, Cetearyl Glucoside, Dimethicone, PEG-100 Stearate, Hydroxyethyl Acrylate/Sodium Acryloyldimethyl Taurate Copolymer, Polyisobutene, PEG-7 Trimethylolpropane Coconut Ether, Sodium Hyaluronate, Tocopheryl Acetate, Polyether-1, Citric Acid, Oleth-3 Phosphate, Phenoxyethanol, Butylene Glycol, Iodopropynyl Butylcarbamate, Triethoxycaprylylsilane"

product_1_list <-
  process_ingredients(
    product = product_1,
    active_ingredients = active_ingredients_1,
    inactive_ingredients = inactive_ingredients_1
  )

product_2 <- "EltaMD UV Sport Broad-Spectrum SPF 50 Pump (7 oz.)"
active_ingredients_2 <- "Zinc Oxide 9%, Octinoxate 7.5%, Octisalate 5%"
inactive_ingredients_2 <- "Bees Wax, Butylene glycol, Cetyl Dimethicone, Cetyl PEG/PPG-10/1 Dimethicone, Disodium EDTA, Hexyl Laurate, Hydrogenated Castor Oil, Iodopropynyl Butylcarbamate, Isopropyl Palmitate, Octyldodecyl Neopentanoate, Purified Water, Sodium Chloride, Tocopheryl Acetate, Triethoxycaprylylsilane"

product_2_list <-
  process_ingredients(
    product = product_2,
    active_ingredients = active_ingredients_2,
    inactive_ingredients = inactive_ingredients_2
  )

product_3 <- "EltaMD UV Clear Broad-Spectrum SPF 46 (1.7 oz.)"
active_ingredients_3 <- "Zinc Oxide 9.0%, Octinoxate 7.5%"
inactive_ingredients_3 <- "Purified Water, Cyclomethicone, Niacinamide, Octyldodecyl Neopentanoate, Hydroxyethyl Acrylate/Sodium Acryloyldimethyl Taurate Copolymer, Polyisobutene, PEG-7 Trimethylolpropane Coconut Ether, Sodium Hyaluronate, Tocopheryl Acetate, Lactic Acid, Oleth-3 Phosphate, Phenoxyethanol, Butylene Glycol, Iodopropynyl Butylcarbamate, Triethoxycaprylylsilane"

product_3_list <-
  process_ingredients(
    product = product_3,
    active_ingredients = active_ingredients_3,
    inactive_ingredients = inactive_ingredients_3
  )

product_4 <- "VANICREAM SUNSCREEN BROAD SPECTRUM SPF 35 (Old Formula)"
active_ingredients_4 <- "Octinoxate 2.8%, Zinc Oxide 11%"
inactive_ingredients_4 <- "C20-40 alcohols, cetyl PEG/PPG-10/1 dimethicone, cyclohexasiloxane, cyclopentasiloxane, cyclotetrasiloxane, dimethiconol, glycerin, hydrogenated castor oil, magnesium chloride, PEG-30 dipolyhydroxystearate, polypropyl silsesquioxane, purified water, tridecyl neopentanoate, triethoxycaprylylsilane, trimethylsiloxy silicate, ubiquinone (coenzyme Q10)"

product_4_list <-
  process_ingredients(
    product = product_4,
    active_ingredients = active_ingredients_4,
    inactive_ingredients = inactive_ingredients_4
  )




output <-
list(
  `1` = product_1_list,
  `2` = product_2_list,
  `3` = product_3_list,
  `4` = product_4_list) %>%
  transpose() %>%
  map(~map(., create_join_key_col)) %>%
  map(~reduce(., full_join, by = "join_key")) %>%
  map(~select(., -join_key))


product_5 <- "Z-Bar Medicated Cleansing Bar"
active_ingredients_5 <- "pyrithione zinc 2%"
inactive_ingredients_5 <- "sodium cocoyl isethionate, stearic acid, coconut acid, water, sodium isethionate, sodium cocoyl glycinate, sodium chloride, petrolatum, sorbitol, cetearyl alcohol, propanediol, ceteareth-20, simethicone, glyceryl stearate, PEG-30 stearate, sorbic acid"

product_5_list <-
  process_ingredients(
    product = product_5,
    active_ingredients = active_ingredients_5,
    inactive_ingredients = inactive_ingredients_5
  )

product_6 <- "ZP Cleansing Bar with Zinc Pyrithione"
active_ingredients_6 <- "Zinc Pyrithione 2%"
inactive_ingredients_6 <- "Sodium Palmate, Sodium Cocoate, Aqua, Eau, Glycerin, Stearic Acid, Titanium Dioxide, Tocopheryl Acetate (Vitamin E), Vitamin A Palmitate, Pentasodium Pentetate, FD&C Blue No. 1, Sodium Chloride"

product_6_list <-
  process_ingredients(
    product = product_6,
    active_ingredients = active_ingredients_6,
    inactive_ingredients = inactive_ingredients_6
  )

product_7 <- "DermaHarmony 2% Pyrithione Zinc (ZnP) Bar Soap 4 oz"
active_ingredients_7 <- "2% Pyrithione Zinc"
inactive_ingredients_7 <- "Saphonified Oils (from palm and coconut), Water, Glycerin (from vegetable oil), Titanium Dioxide (a natural mineral), Oatmeal, Olive Oil, Vitamin E, Table Salt, Salt of Pentetic Acid"

product_7_list <-
  process_ingredients(
    product = product_7,
    active_ingredients = active_ingredients_7,
    inactive_ingredients = inactive_ingredients_7
  )

output <-
  list(
    `1` = product_5_list,
    `2` = product_6_list,
    `3` = product_7_list) %>%
  transpose() %>%
  map(~map(., create_join_key_col)) %>%
  map(~reduce(., full_join, by = "join_key")) %>%
  map(~select(., -join_key))
