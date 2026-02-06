# Generated from HWSD_META.mdb on 2026-02-06
.hwsd_meta_maps <- function() {
  if (!is.null(.tidyhwsd_cache$meta_maps)) {
    return(.tidyhwsd_cache$meta_maps)
  }

  maps <- list(
    D_ADD_PROP = data.frame(
      code = c("0", "1", "2", "3"),
      label = c("None", "Petric", "Gelic", "Vertic"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_AWC = data.frame(
      code = c("1", "2", "3", "4", "5", "6", "7"),
      label = c("150", "125", "100", "75", "50", "15", "0"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_COVERAGE = data.frame(
      code = c("0", "1", "2", "3", "4"),
      label = c("None", "ESDB", "CHINA", "SOTWIS", "DSMW"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_DRAINAGE = data.frame(
      code = c("1", "2", "3", "4", "5", "6", "7"),
      label = c("Very Poor", "Poor", "Imperfectly", "Moderately Well", "Well", "Somewhat Excessive", "Excessive"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_IL = data.frame(
      code = c("0", "1", "2", "3", "4"),
      label = c("None", "> 150", "80-150", "40-80", "< 40"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_ISSOIL = data.frame(
      code = c("0", "1"),
      label = c("Non-soil", "Yes"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_PHASE = data.frame(
      code = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30"),
      label = c("No information", "Stony", "Lithic", "Petric", "Petrocalcic", "Petrogypsic", "Petroferric", "Phreatic", "Fragipan", "Duripan", "Saline", "Sodic", "Cerrado", "Anthraquic", "Gelundic", "Gigai", "Inundic", "Placic", "Rudic", "Salic", "Skeletic", "Takyric", "Yermic", "Erosion", "No limitation to agricultural use", "Gravelly", "Concretionary", "Glaciers", "Soils disturbed by man", "Excessively drained", "Flooded"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_ROOTS = data.frame(
      code = c("0", "1", "2", "3", "4", "5", "6"),
      label = c("None", ">80", "60-80", "40-60", "20-40", "0-80", "0-20"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_SWR = data.frame(
      code = c("0", "1", "2", "3", "4"),
      label = c("None", "Wet:  (0-80 cm) < 3 months; (0-40cm) < 1 month", "Wet:  (0-80 cm) 3-6 months; (0-40cm) < 1 month", "Wet:  (0-80 cm) > 6 months; 0-40 cm > 11 months", "Wet:   0-40 cm > 11 months"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_TEXTURE = data.frame(
      code = c("0", "1", "2", "3"),
      label = c("None", "Coarse", "Medium", "Fine"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_USDA_TEX_CLASS = data.frame(
      code = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
      label = c("clay(heavy)", "silty clay", "clay (light)", "silty clay loam", "clay loam", "silt", "silt loam", "sandy clay", "loam", "sandy clay loam", "sandy loam", "loamy sand", "sand"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_SYMBOL = data.frame(
      code = c("AC", "AL", "AN", "AR", "AT", "CH", "CL", "CM", "FL", "FR", "GL", "GR", "GY", "HS", "KS", "LP", "LV", "LX", "NT", "PD", "PH", "PL", "PT", "PZ", "RG", "SC", "SN", "VR", "RK", "DS", "WR", "UR", "ST", "NI", "GG", "IS"),
      label = c("Acrisols", "Alisols", "Andosols", "Arenosols", "Anthrosols", "Chernozems", "Calcisols", "Cambisols", "Fluvisols", "Ferralsols", "Gleysols", "Greyzems", "Gypsisols", "Histosols", "Kastanozems", "Leptosols", "Luvisols", "Lixisols", "Nitisols", "Podzoluvisols", "Phaeozems", "Planosols", "Plinthosols", "Podzols", "Regosols", "Solonchaks", "Solonetz", "Vertisols", "Rock Outcrop", "Sand Dunes", "Water Bodies", "Urban, mining, etc.", "Salt Flats", "No data", "Glaciers", "Island"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_SYMBOL74 = data.frame(
      code = c("G", "Ge", "Gc", "Gd", "Gm", "Gh", "Gp", "Gx", "R", "Re", "Rc", "Rd", "Rx", "I", "Q", "Qc", "Ql", "Qf", "Qa", "E", "U", "T", "To", "Tm", "Th", "Tv", "V", "Vp", "Vc", "Z", "Zo", "Zm", "Zt", "Zg", "S", "So", "Sm", "Sg", "Y", "Yh", "Yk", "Yy", "Yl", "Yt", "X", "Xh", "Xk", "Xy", "Xl", "K", "Kh", "Kk", "Kl", "C", "Ch", "Ck", "Cl", "Cg", "H", "Hh", "Hc", "Hl", "Hg", "M", "Mo", "Mg", "B", "Be", "Bd", "Bh", "Bg", "Bx", "Bk", "Bc", "Bv", "Bf", "L", "Lo", "Lc", "Lk", "Lv", "Lf", "La", "Lp", "Lg", "D", "De", "Dd", "Dg", "P", "Po", "Pl", "Pf", "Ph", "Pp", "Pg", "W", "We", "Wd", "Wm", "Wh", "Ws", "Wx", "A", "Ao", "Af", "Ah", "Ap", "Ag", "N", "Ne", "Nd", "Nh", "F", "Fo", "Fx", "Fr", "Fh", "Fa", "Fp", "O", "Oe", "Od", "Ox", "ST", "GG", "WR", "??", "DS", "RK", "J", "Je", "Jc", "Jd", "Jt", "NI"),
      label = c("Gleysols", "Eutric Gleysols", "Calcaric Gleysols", "Dystric Gleysols", "Mollic Gleysols", "Humic Gleysols", "Plinthic Gleysols", "Gelic Gleysols", "Regosols", "Eutric Regosols", "Calcaric Regosols", "Dystric Regosols", "Gelic Regosols", "Lithosols", "Arenosols", "Cambic Arenosols", "Luvic Arenosols", "Ferralic Arenosols", "Albic Arenosols", "Rendzinas", "Rankers", "Andosols", "Ochric Andosols", "Mollic Andosols", "Humic Andosols", "Vitric Andosols", "Vertisols", "Pellic Vertisols", "Chromic Vertisols", "Solonchaks", "Orthic Solonchaks", "Mollic Solonchaks", "Takyric Solonchaks", "Gleyic Solonchaks", "Solonetz", "Orthic Solonetz", "Mollic Solonetz", "Gleyic Solonetz", "Yermosols", "Haplic Yermosols", "Calcic Yermosols", "Gypsic Yermosols", "Luvic Yermosols", "Takyric Yermosols", "Xerosols", "Haplic Xerosols", "Calcic Xerosols", "Gypsic Xerosols", "Luvic Xerosols", "Kastanozems", "Haplic Kastanozems", "Calcic Kastanozems", "Luvic Kastanozems", "Chernozems", "Haplic Chernozems", "Calcic Chernozems", "Luvic Chernozems", "Glossic Chernozems", "Phaeozems", "Haplic Phaeozems", "Calcaric Phaeozems", "Luvic Phaeozems", "Gleyic Phaeozems", "Greyzems", "Orthic Greyzems", "Gleyic Greyzems", "Cambisols", "Eutric Cambisols", "Dystric Cambisols", "Humic Cambisols", "Gleyic Cambisols", "Gelic Cambisols", "Calcic Cambisols", "Chromic Cambisols", "Vertic Cambisols", "Ferralic Cambisols", "Luvisols", "Orthic Luvisols", "Chromic Luvisols", "Calcic Luvisols", "Vertic Luvisols", "Ferric Luvisols", "Albic Luvisols", "Plinthic Luvisols", "Gleyic Luvisols", "Podzoluvisols", "Eutric Podzoluvisols", "Dystric Podzoluvisol", "Gleyic Podzoluvisols", "Podzols", "Orthic Podzols", "Leptic Podzols", "Ferric Podzols", "Humic Podzols", "Placic Podzols", "Gleyic Podzols", "Planosols", "Eutric Planosols", "Dystric Planosols", "Mollic Planosols", "Humic Planosols", "Sodic Planosols", "Gelic Planosols", "Acrisols", "Orthic Acrisols", "Ferric Acrisols", "Humic Acrisols", "Plinthic Acrisols", "Gleyic Acrisols", "Nitosols", "Eutric Nitosols", "Dystric Nitosols", "Humic Nitosols", "Ferralsols", "Orthic Ferralsols", "Xanthic Ferralsols", "Rhodic Ferralsols", "Humic Ferralsols", "Acric Ferralsols", "Plinthic Ferralsols", "Histosols", "Eutric Histosols", "Dystric Histosols", "Gelic Histosols", "Salt flats", "Glaciers", "Water bodies", "???", "Dunes/Sand", "Rock Outcrops", "Fluvisols", "Eutric Fluvisols", "Calcaric Fluvisols", "Dystric Fluvisols", "Thionic Fluvisols", "No Information"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_SYMBOL85 = data.frame(
      code = c("A", "Af", "Ag", "Ah", "Ao", "Ap", "B", "Ba", "Bc", "Bcc", "Bch", "Bck", "Bcr", "Bd", "Bda", "Bdg", "Bds", "Be", "Bea", "Bec", "Bef", "Beg", "Bev", "Bf", "Bg", "Bgc", "Bge", "Bgg", "Bgs", "Bgv", "Bh", "Bk", "Bkf", "Bkh", "Bkv", "Bm", "Bv", "Bvc", "Bvk", "Bx", "C", "Cgs", "Ch", "Chv", "Ck", "Ckb", "Ckc", "Ckcb", "Cl", "D", "Dd", "De", "Dg", "Dgd", "Dge", "Dgs", "E", "Ec", "Eh", "Eo", "Eu", "G", "Gc", "Gcf", "Gcs", "Gd", "Gds", "Ge", "Gef", "Geh", "Ges", "Gev", "Gf", "Gfm", "Gh", "Ghf", "Ghh", "Ghhc", "Ght", "Gi", "Gih", "Gl", "Gls", "Gm", "Gmc", "Gmf", "Gms", "Gs", "Gtz", "Gx", "H", "Hc", "Hcb", "Hcf", "Hcg", "Hcn", "Hg", "Hgc", "Hgs", "Hgv", "Hh", "Hhv", "Hl", "Hlv", "Ho", "I", "Ic", "Ich", "Id", "Ie", "J", "Jc", "Jcf", "Jcg", "Jd", "Jdg", "Je", "Jef", "Jeg", "Jm", "Jmg", "Jmv", "Jt", "Kh", "Kk", "Kkb", "Kkv", "Kl", "Ko", "L", "La", "Lc", "Lcp", "Lcr", "Lcv", "Ld", "Ldg", "Lf", "Lg", "Lga", "Lgp", "Lgs", "Lh", "Lk", "Lkc", "Lkcr", "Lkv", "Lo", "Lop", "Lp", "Lv", "Lvc", "Lvk", "Mo", "O", "Od", "Odp", "Oe", "Ox", "P", "Pf", "Pg", "Pgh", "Pgs", "Ph", "Phf", "Pl", "Plh", "Po", "Pof", "Poh", "Pp", "Q", "Qa", "Qc", "Qcc", "Qcd", "Qcg", "Qcs", "Qh", "Ql", "Qld", "Qlg", "R", "Rc", "Rd", "Rds", "Re", "Rx", "Sg", "Sm", "So", "Sof", "Th", "Tm", "To", "Tv", "U", "Ud", "Uk", "V", "Vc", "Vcc", "Vg", "Vp", "Vpc", "Vpg", "Vpn", "W", "Wd", "Wdv", "We", "Wev", "Wh", "Wm", "Ws", "Xk", "Xl", "Xy", "Z", "Zg", "Zgf", "Zo", "Zt", "PS", "RK", "HD", "UR", "MA", "WR", "GG", "NS", "NI"),
      label = c("Acrisol", "Ferric Acrisol", "Gleyic Acrisol", "Humic Acrisol", "Orthic Acrisol", "Plinthic Acrisol", "Cambisol", "Calcaric Cambisol", "Chromic Cambisol", "Calcaro-chromic Cambisol", "Humo-chromic Cambisol", "Calci-chromic Cambisol", "Rhodo-chromic Cambisol", "Dystric Cambisols", "Ando-dystric Cambisol", "Gleyo-eutric Cambisol", "Spodo-dystric Cambisol", "Eutric Cambisol", "Ando-eutric Cambisol", "Calcaro-eutric Cambisol", "Fluvi-eutric Cambisol", "Gleyo-eutric Cambisol", "Verti-eutric Cambisol", "Ferralic Cambisol", "Gleyic Cambisol", "Calcaro-gleyic Cambisol", "Eutri-gleyic Cambisol", "Stagno-gleyic Cambisol", "Spodo-gleyic Cambisol", "Verti-gleyic Cambisol", "Humic Cambisol", "Calcic Cambisol", "Fluvi-calcic Cambisol", "Humo-calcic Cambisol", "Calci-vertic Cambisol", "Mollic Cambisol", "Vertic Cambisol", "Calcaro-vertic Cambisol", "Calci-vertic Cambisol", "Gelic Cambisol", "Chernozem", "Stagno-gleyic Chernozem", "Haplic Chernozem", "Verti-haplic Chernozem", "Calcic Chernozem", "Vermi-calcic Chernozem", "Calcaro-calcic Chernozem", "Vermi-calcaro-calcic Chernozem", "Luvic Chernozem", "Podzolluvisol", "Dystric Podzoluvisol", "Eutric Podzoluvisol", "Gleyic Podzoluvisol", "Dystri-gleyic Podzoluvisol", "Eutri-gleyic Podzoluvisol", "Stagno-gleyic Podzoluvisol", "Rendzina", "Cambic Rendzina", "Histic Rendzina", "Orthic Rendzina", "Umbric Rendzina", "Gleysol", "Calcaric Gleysol", "Fluvi-calcaric Gleysol", "Stagno-calcaric Gleysol", "Dystric Gleysol", "Stagno-dystric Gleysol", "Eutric Gleysol", "Fluvi-eutric Gleysol", "Humi-eutric Gleysol", "Stagno-eutric Gleysol", "Verti-eutric Gleysol", "Fluvic Gleysol", "Molli-fluvic Gleysol", "Humic Gleysol", "Fluvi-humic Gleysol", "Histo-humic Gleysol", "Histo-humo-calcaric Gleysol", "Thioni-humic Gleysol", "Histic Gleysol", "Humo-histic Gleysol", "Luvic Gleysol", "Stagno-luvic Gleysol", "Mollic Gleysols", "Calcaro-mollic Gleysol", "Fluvi-mollic Gleysol", "Stagno-mollic Gleysol", "Stagnic Gleysol", "Thionic Gleysols", "Gelic Gleysol", "Phaeozem", "Calcaric Phaeozem", "Vermi-calcaric Phaeozem", "Fluvi-calcaric Phaeozem", "Gleyo-calcaric Phaeozem", "Alkalino-Calcaric Phaeozem", "Gleyic Phaeozems", "Calcaro-gleyic Phaeozem", "Stagno-gleyic Phaeozem", "Verti-gleyic Phaeozem", "Haplic Phaeozem", "Verti-haplic Phaeozem", "Luvic Phaeozems", "Verti-luvic Phaeozem", "Haplic Phaeozem", "Lithosol", "Calcaric Lithosol", "Humo-calcaric Lithosol", "Dystric Lithosol", "Eutric Lithosol", "Fluvisol", "Calcaric Fluvisol", "Fluvi-calcaric Fluvisol", "Gleyo-calcaric Fluvisol", "Dystric Fluvisol", "Gleyo-dystric Fluvisol", "Eutric Fluvisols", "Fluvi-eutric Fluvisol", "Gleyo-eutric Fluvisol", "Mollic Fluvisols", "Gleyo-mollic Fluvisol", "Verti-mollic Fluvisol", "Thionic Fluvisol", "Haplic Kastanozem", "Calcic Kastanozem", "Vermi-calcic Kastanozem", "Verti-calcic Kastanozem", "Luvic Kastanozems", "Orthic Kastanozems", "Luvisol", "Albic Luvsol", "Chromic Luvisol", "Plano-chromic Luvisol", "Rhodo-chromic Luvisol", "Chromo-vertic Luvisol", "Dystric Luvisol", "Gleyo-dystric Luvisol", "Ferric Luvisol", "Gleyic Luvisol", "Albo-gleyic Luvisol", "Plano-gleyic Luvisol", "Stagno-gleyic Luvisol", "Humic Luvisol", "Calcic Luvisol", "Chromo-calcic Luvisol", "Rhodo-chromo-calcic Luvisol", "Verti-calcic Luvisol", "Orthic Luvisol", "Plano-orthic Luvisol", "Plinthic Luvisol", "Vertic Luvisols", "Chromo-vertic Luvisol", "Calci-vertic Luvisol", "Orthic Greyzem", "Histosol", "Dystric Histosol", "Placi-dystric Histosol", "Eutric Histosol", "Gelic Histosol", "Podzol", "Ferric Podzol", "Gleyic Podzol", "Histo-gleyic Podzol", "Stagno-gleyic Podzol", "Humic Podzol", "Ferro-humic Podzol", "Leptic Podzol", "Humo-leptic Podzol", "Orthic Podzols", "Ferro-orthic Podzol", "Humo-orthic Podzol", "Placic Podzol", "Arenosol", "Albic Arenosol", "Cambic Arenosol", "Calcaro-cambicx Arenosol", "Dystri-cambic Arenosol", "Gleyo-cambic Arenosol", "Spodo-cambic Arenosol", "Haplic Arenosol", "Luvic Arenosols", "Dystri-luvic Arenosol", "Gleyo-luvic Arenosol", "Regosol", "Calcaric Regosol", "Dystric Regosol", "Stagno-dystric Regosol", "Eutric Regosol", "Gelic Regosol", "Gleyic Solonetz", "Mollic Solonetz", "Orthic Solonetz", "Fluvi-orthic Solonetz", "Humic Andosol", "Mollic Andosol", "Haplic Andosol", "Vitric Andosol", "Ranker", "Dystric Ranker", "Calcaric Ranker", "Vertisol", "Chromic Vertisol", "Calcaro-chromic Vertisol", "Gleyic Vertisol", "Pellic Vertisol", "Calcaro-pellic Vertisol", "Gleyo-pellic Vertisol", "Sodi-pellic Vertisol", "Planosol", "Dystric Planosol", "Verti-dystric Planosol", "Eutric Planosols", "Verti-eutric Planosol", "Humic Planosol", "Mollic Planosol", "Solodic Planosol", "Calcic Xerosol", "Luvic Xerosol", "Gypsic Xerosol", "Solonchak", "Gleyic Solonchak", "Fluvi-gleyic Slonchak", "Orthic Solonchak", "Thionic Solonchak", "Plaggensol", "Rock outcrops", "Human disturbed soil", "Urban, mining, etc.", "Marsh", "Water bodies", "Glaciers", "not surveyed", "No Data"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    D_SYMBOL90 = data.frame(
      code = c("FL", "FLe", "FLc", "FLd", "FLm", "FLu", "FLt", "FLs", "GL", "GLe", "GLk", "GLd", "GLa", "GLm", "GLu", "GLt", "GLi", "AC", "ACh", "ACf", "ACu", "ACp", "ACg", "AL", "ALh", "ALf", "ALu", "ALp", "ALj", "ALg", "AN", "ANh", "ANm", "ANu", "ANz", "ANg", "ANi", "AR", "ARh", "ARb", "ARl", "ARo", "ARa", "ARc", "ARg", "AT", "ATa", "ATc", "ATf", "ATu", "CH", "CHh", "CHk", "CHl", "CHw", "CHg", "CL", "CLh", "CLl", "CLp", "CM", "CMe", "CMd", "CMu", "CMc", "CMx", "CMv", "CMo", "CMg", "CMi", "FR", "FRh", "FRx", "FRr", "FRu", "FRg", "FRp", "GR", "GRh", "GRg", "GY", "GYh", "GYk", "GYl", "GYp", "HS", "HSl", "HSs", "HSf", "HSt", "HSi", "KS", "KSh", "KSl", "KSk", "KSy", "LP", "LPe", "LPd", "LPk", "LPm", "LPu", "LPq", "LPi", "LV", "LVh", "LVf", "LVx", "LVk", "LVv", "LVa", "LVj", "LVg", "LX", "LXh", "LXf", "LXp", "LXa", "LXj", "LXg", "NT", "NTh", "NTr", "NTu", "PD", "PDe", "PDd", "PDj", "PDg", "PDi", "PH", "PHh", "PHc", "PHl", "PHj", "PHg", "PL", "PLe", "PLd", "PLm", "PLu", "PLi", "PT", "PTe", "PTd", "PTu", "PTa", "PZ", "PZh", "PZb", "PZf", "PZc", "PZg", "PZi", "RG", "RGe", "RGc", "RGy", "RGd", "RGu", "RGi", "SC", "SCh", "SCm", "SCg", "SCk", "SCy", "SCn", "SCi", "SN", "SNh", "SNm", "SNk", "SNy", "SNj", "SNg", "VR", "VRe", "VRd", "VRk", "VRy", "DS", "ST", "RK", "WRs", "WR", "GG", "ND", "UR", "HD", "MA", "FP", "IS"),
      label = c("FLUVISOLS", "Eutric Fluvisols", "Calcaric Fluvisols", "Dystric Fluvisols", "Mollic Fluvisols", "Umbric Fluvisols", "Thionic Fluvisols", "Salic Fluviosls", "GLEYSOLS", "Eutric Gleysols", "Calcic Gleysols", "Dystric Gleysols", "Andic Gleysols", "Mollic Gleysols", "Umbric Gleysols", "Thionic Gleysols", "Gelic Gleysols", "ACRISOLS", "Haplic Acrisols", "Ferric Acrisols", "Humic Acrisols", "Plinthic Acrisols", "Gleyic Acrisols", "ALISOLS", "Haplic Alisols", "Ferric Alisols", "Humic Alisols", "Plinthic Alisols", "Stagnic Alisols", "Gleyic Alisols", "ANDOSOLS", "Haplic Andosols", "Mollic Andosols", "Umbric Andosols", "Vitric Andosols", "Gleyic Andosols", "Gelic Andosols", "ARENOSOLS", "Haplic Arenosols", "Cambic Arenosols", "Luvic Arenosols", "Ferralic Arenosols", "Albic Arenosols", "Calcaric Arenosols", "Gleyic Arenosols", "ANTHROSOLS", "Aric Anthrosols", "Cumulic Anthrosols", "Fimic Anthrosols", "Urbic Anthrosols", "CHERNOZEMS", "Haplic Chernozems", "Calcic Chernozems", "Luvic Chernozems", "Glossic Chernozems", "Gleyic Chernozems", "CALCISOLS", "Haplic Calcisols", "Luvic Calcisols", "Petric Calcisols", "CAMBISOLS", "Eutric Cambisols", "Dystric Cambisols", "Humic Cambisols", "Calcaric Cambisols", "Chromic Cambisols", "Vertic Cambisols", "Ferralic Cambisols", "Gleyic Cambisols", "Gelic Cambisols", "FERRALSOLS", "Haplic Ferralsols", "Xanthic Ferralsols", "Rhodic Ferralsols", "Humic Ferralsols", "Geric Ferralsols", "Plinthic Ferralsols", "GREYZEMS", "Haplic Greyzems", "Gleyic Greyzems", "GYPSISOLS", "Haplic Gypsisols", "Calcic Gypsisols", "Luvic Gypsisols", "Petric Gypsisols", "HISTOSOLS", "Folic Histosols", "Terric Histosols", "Fibric Histosols", "Thionic Histosols", "Gelic Histosols", "KASTANOZEMS", "Haplic Kastanozems", "Luvic Kastanozems", "Calcic Kastanozems", "Gypsic Kastanozems", "LEPTOSOLS", "Eutric Leptosols", "Dystric Leptosols", "Rendzic Leptosols", "Mollic Leptosols", "Umbric Leptosols", "Lithic Leptosols", "Gelic Leptosols", "LUVISOLS", "Haplic Luvisols", "Ferric Luvisols", "Chromic Luvisols", "Calcic Luvisols", "Vertic Luvisols", "Albic Luvsiols", "Stagnic Luvisols", "Gleyic Luvisols", "LIXISOLS", "Haplic Lixisols", "Ferric Lixisols", "Plinthic Lixisols", "Albic Lixisols", "Stagnic Lixisols", "Gleyic Lixisols", "NITISOLS", "Haplic Nitisols", "Rhodic Nitisols", "Humic Nitisols", "PODZOLUVISOLS", "Eutric Podzoluvisols", "Dystric Podzoluvisols", "Stagnic Podzoluvisols", "Gleyic Podzoluvisols", "Gelic Podzoluvisols", "PHAEOZEMS", "Haplic Phaeozems", "Calcaric Phaeozems", "Luvic Phaeozems", "Stagnic Phaeozems", "Gleyic Phaeozems", "PLANOSOLS", "Eutric Planosols", "Dystric Planosols", "Mollic Planosols", "Umbric Planosols", "Gelic Planosols", "PLINTHOSOLS", "Eutric Plinthosols", "Dystric Plinthosols", "Humic Plinthosols", "Albic Plinthosols", "PODZOLS", "Haplic Podzols", "Cambic Podzols", "Ferric Podzols", "Carbic Podzols", "Gleyic Podzols", "Gelic Podzols", "REGOSOLS", "Eutric Regosols", "Calcaric Regosols", "Gypsic Regosols", "Dystric Regosols", "Umbric Regosols", "Gelic Regosols", "SOLONCHAKS", "Haplic Solonchaks", "Mollic Solonchaks", "Gleyic Solonchaks", "Calcic Solonchaks", "Gypsic Solonchaks", "Sodic Solonchaks", "Gelic Solonchaks", "SOLONETZ", "Haplic Solonetz", "Mollic Solonetz", "Calcic Solonetz", "Gypsic Solonetz", "Stagnic Solonetz", "Gleyic Solonetz", "VERTISOLS", "Eutric Vertisols", "Dystric Vertisols", "Calcic Vertisols", "Gypsic Vertisols", "Dunes & shift.sands", "Salt flats", "Rock outcrops", "Inland water, salt", "Water bodies", "Glaciers", "No Data", "Urban, mining, etc.", "Humanly disturbed", "Marsh", "Fishpond", "Island"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )

  .tidyhwsd_cache$meta_maps <- maps
  maps
}

.hwsd_normalize_codes <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(num <- as.numeric(x_chr))
  is_int <- !is.na(num) & abs(num - round(num)) < 1e-8
  x_chr[is_int] <- as.character(as.integer(round(num[is_int])))
  x_chr
}

.hwsd_levels_from_meta <- function(map_df, x) {
  if (is.null(map_df) || nrow(map_df) == 0) {
    return(NULL)
  }

  codes_map <- .hwsd_normalize_codes(map_df$code)
  labels_map <- as.character(map_df$label)

  keep <- !is.na(codes_map)
  codes_map <- codes_map[keep]
  labels_map <- labels_map[keep]

  if (length(codes_map) == 0) {
    return(NULL)
  }

  dup <- duplicated(codes_map)
  if (any(dup)) {
    codes_map <- codes_map[!dup]
    labels_map <- labels_map[!dup]
  }

  data_codes <- .hwsd_normalize_codes(x)
  data_codes <- unique(data_codes[!is.na(data_codes)])

  extra <- setdiff(data_codes, codes_map)
  codes <- c(codes_map, extra)
  labels <- c(labels_map, extra)

  data.frame(
    ID = seq_along(codes),
    code = codes,
    label = labels,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.hwsd_meta_key <- function(col) {
  switch(col,
    DRAINAGE = "D_DRAINAGE",
    ADD_PROP = "D_ADD_PROP",
    ROOTS = "D_ROOTS",
    IL = "D_IL",
    ROOT_DEPTH = "D_IL",
    SWR = "D_SWR",
    PHASE1 = "D_PHASE",
    PHASE2 = "D_PHASE",
    ISSOIL = "D_ISSOIL",
    TEXTURE_USDA = "D_USDA_TEX_CLASS",
    WRB2 = "D_SYMBOL",
    WRB4 = "D_SYMBOL",
    FAO90 = "D_SYMBOL90",
    COVERAGE = "D_COVERAGE",
    NULL
  )
}

.hwsd_drainage_levels <- function(x) {
  maps <- .hwsd_meta_maps()
  base <- maps$D_DRAINAGE
  if (is.null(base) || nrow(base) == 0) {
    return(NULL)
  }

  labels <- as.character(base$label)
  abbr <- c("VP", "P", "I", "MW", "W", "SE", "E")

  codes <- .hwsd_normalize_codes(x)
  codes <- codes[!is.na(codes)]

  if (any(codes %in% abbr)) {
    map_df <- data.frame(
      code = abbr,
      label = labels,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    return(.hwsd_levels_from_meta(map_df, x))
  }

  if (any(codes %in% labels)) {
    map_df <- data.frame(
      code = labels,
      label = labels,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    return(.hwsd_levels_from_meta(map_df, x))
  }

  .hwsd_levels_from_meta(base, x)
}
