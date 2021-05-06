setwd('~/Desktop/OccupancyModelingEffectofCEI')

d0 <- '1_Cleandata_and_makeMCPs'
dir.create(d0)
for(d1 in c('mcp',
            'pts',
            'pts_rds',
            'ptsprj',
            'ptsprj_rds')) {
  dir.create(file.path(d0, d1))
  for(d2 in c('t0', 't1', 't2', 't3')) {
    dir.create(file.path(d0, d1, d2))
  }
}

d0 <- '2_CalcSpeciesPr_Rich'
dir.create(d0)
for(d1 in c('bee_pr',
            'bee_prab',
            'bee_prab_at3',
            'bee_prab_at5',
            'bee_pts',
            'bee_sprich')) {
  dir.create(file.path(d0, d1))
  if(d1 %in% c('bee_pr', 'bee_pts'))
    for(d2 in c('season_0_1',
                'season_0_2',
                'season_0_3',
                'season_3_1',
                'season_3_2',
                'season_3_3')) {
      dir.create(file.path(d0, d1, d2))
    }
  if(d1 %in% c('bee_prab', 'bee_prab_at3', 'bee_prab_at5'))
    for(d2 in c('p1',
                'p2')) {
      dir.create(file.path(d0, d1, d2))
    }
  if(d1 %in% c('bee_sprich'))
    for(d2 in c('sprich_p1', 'sprich_p2')) {
      dir.create(file.path(d0, d1, d2))
    }
}

dir.create('3_CalcSamplingEffort_Cont')
dir.create('4_CalcClim_andExposure')
dir.create('5_binomialGLMM4Presence')
dir.create('6_ModelRawSpRich')
dir.create('7_shapeOccudata')
d0 <- '8_ModelOccupancy'
dir.create(d0)
for(d1 in c('occu_p1',
            'occu_p1_sd',
            'occu_p2',
            'occu_p2_sd',
            'occufits')) {
  dir.create(file.path(d0, d1))
}
dir.create('9_modelOccupancy2CEI')
dir.create('10_modelOccupancyRichness')







