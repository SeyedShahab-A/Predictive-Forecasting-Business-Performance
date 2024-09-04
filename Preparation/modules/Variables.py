import pandas as pd
import os
from modules.Cost_centre_structure import *

def get_region(sheet_name, subregion_dict):
    for region, cost_centres in subregion_dict.items():
        if sheet_name in cost_centres:
            return region
    return None

def get_metadata(data, division, sheet_name):
    region, cost_centre =  None, None

    # Find the appropriate subregion dictionary for the division
    subregion_dict = division_to_subregion.get(division)
    if subregion_dict:
        region = get_region(sheet_name, subregion_dict)

    # Format cost centre
    division_abbr = division_ab.get(division, division)
    formatted_sheet_name = sheet_name.replace(' ', '_')
    cost_centre = f"{division_abbr}_{formatted_sheet_name}"


    data['Region'] = region
    data['Division'] = division.capitalize()
    data['Cost_Centre'] = cost_centre

    return data, region, division, cost_centre



def extract_var_columns(data, start_col, end_col):

    variables_to_extract = [
        "secured", "probable", "possible", "speculative", "biso", "budget income",
        "Other Direct Costs", "Direct Costs", "gross profit", "budget gross profit",
        "overheads", "subcon profit/(loss)", "net profit", "budget profit", "income / hd", "cost / hd",
        "t&t staff costs", "contract staff costs", "expenses", "t&t technical staff",
        "contract staff", "total headcount"
    ]

    var_dict = {"budget income": "budget_income", "other direct costs": "other_direct_costs", "direct costs":
        "direct_costs","gross profit":"gross_profit","budget gross profit":"budget_gross_profit", 'net profit': 'net_profit',
                "income / hd":"income/hd","cost / hd":"cost/hd","t&t staff costs":"t_staff_costs", "budget profit": "budget_profit",
                "contract staff costs": "contract_staff_costs","t&t technical staff":"t_technical_staff","contract staff"
                :"contract_staff","total headcount":"total_headcount", 'subcon profit/(loss)': 'subcon'}

    variables_to_extract = [var.strip().lower() for var in variables_to_extract]

    # Initialize an empty dictionary to hold data for each variable
    data_dict = {var: [] for var in variables_to_extract}

    # Standardize the text by converting to lower case and stripping extra spaces
    data_r = data.applymap(lambda x: x.strip().lower() if isinstance(x, str) else x)

    # Extract data for these variables, maintaining row order
    for index, row in data_r.iterrows():
        if row[5] in variables_to_extract:
            data_dict[row[5]].append(row[start_col:end_col+1].values)
        if row[6] in variables_to_extract:
            data_dict[row[6]].append(row[start_col:end_col+1].values)
        if row[7] in variables_to_extract:
            data_dict[row[7]].append(row[start_col:end_col+1].values)
        if row[8] in variables_to_extract:
            data_dict[row[8]].append(row[start_col:end_col+1].values)

    # Combine data into a structured dataframe
    extracted_data = pd.DataFrame()
    for var in variables_to_extract:
        if data_dict[var]:
            extracted_data[var] = pd.Series(data_dict[var][0]).astype(float, errors='raise')

    extracted_data.rename(columns=var_dict, inplace=True)

    extracted_data['t_staff_costs'] = -extracted_data['t_staff_costs']
    extracted_data['contract_staff_costs'] = -extracted_data['contract_staff_costs']
    extracted_data['direct_costs'] = -extracted_data['direct_costs']
    extracted_data['other_direct_costs'] = -extracted_data['other_direct_costs']
    extracted_data['cost/hd'] = -extracted_data['cost/hd']
    extracted_data['overheads'] = -extracted_data['overheads']
    extracted_data['expenses'] = -extracted_data['expenses']

    return extracted_data
#%%
