# Cost_centre_structure.py
CM_list = ['Edinburgh','Glasgow','Belfast','NorthEast','Leeds','NorthWest',
           'Sheffield','Birmingham','Nottingham','Bristol','Cambridge',
           'CLGov','HE','Occupier','HiTech','Developer','alinea','Co']

CM_sub_region = {'other_regions': ['Edinburgh','Glasgow','Belfast','NorthEast','Leeds','NorthWest',
                            'Sheffield','Birmingham','Nottingham','Bristol'],
                 'London': ['Cambridge','CLGov','HE','Occupier','HiTech','Developer','alinea'],
                 'Co': ['Co']}

INF_sub_region = {'Scotland':['CM Scotland','PM Scotland','PMO Scotland'],
                  'NW':['CM NW','PM NW','PMO NW','Def Nth'],
                  'YNE':['CM YNE','PM YNE','PMO YNE'],
                  'Midlands':['CM Midlands','PM Midlands','PMO Midlands'],
                  'SE':['CM SE','PM SE','PMO SE','Def SE'],
                  'SW':['T&U SW', 'Def CM SW','Def P3M SW','MOD'],
                  'Co':['Co'],
                  'PA':['PA'],
                  'Digital':['Digital']
                  }
NR_sub_region = {'NR North': ['NR North'],
                 'PF': ['PF']
                }

PM_sub_region = {'Edinburgh':['Edinburgh'],
                 'Glasgow':['Glasgow'],
                 'Belfast':['Belfast'],
                 'North East':['North East'],
                 'Leeds':['Leeds'],
                 'North West':['North West'],
                 'Project Controls':['Project Controls'],
                 'Sheffield':['Sheffield'],
                 'Birmingham':['Birmingham'],
                 'Nottingham':['Nottingham'],
                 'Bristol':['Bristol'],
                 'London':['Cambridge','CLG','Hitech','HE','Occupier','Developer','Pcon Sth','PMO'],
                 'RE Digital':['RE Digital'],
                 'Co':['Co']
                 }

ADV_sub_region = {'Consulting':['AFM','Pro'],
                  'Sustainability':['R&H','NZ'],
                  'CS':['Proc','Disp'],
                  'SHQ':['SHQUK','SHQTel'],
                  'Suiko':['Suiko'],
                  'Co':['AdvCo']
                  }

AMCL_sub_region = {'Group':['Group'],
                   'UK&I':['UK&I'],
                   'AMA':['AMA'],
                   'Vertex':['Vertex'],
                   'IES':['IES']
                   }

division_to_subregion = {
    'cost management': CM_sub_region,
    'infrastructure': INF_sub_region,
    'natural resources': NR_sub_region,
    'project management': PM_sub_region,
    'advisory': ADV_sub_region,
    'amcl': AMCL_sub_region
}

division_ab = {
    'cost management':'CM',
    'infrastructure':'INF',
    'natural resources':'NR',
    'project management':'PM',
    'advisory':'ADV',
    'amcl':'AMCL'
}

