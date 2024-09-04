import pandas as pd
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
        "contract staff", "total headcount", 'total income'
    ]

    var_dict = {"budget income": "budget_income", "other direct costs": "other_direct_costs", "direct costs": "direct_costs",
                "gross profit": "gross_profit", "budget gross profit": "budget_gross_profit", "net profit": "net_profit",
                "income / hd": "income/hd", "cost / hd": "cost/hd", "t&t staff costs": "t_staff_costs", "budget profit": "budget_profit",
                "contract staff costs": "contract_staff_costs", "t&t technical staff": "t_technical_staff", "contract staff": "contract_staff",
                "total headcount": "total_headcount", "subcon profit/(loss)": "subcon", 'total income': 'total_income'}

    variables_to_extract = [var.strip().lower() for var in variables_to_extract]

    # Initialize an empty DataFrame with all variables set to None
    extracted_data = pd.DataFrame(index=range(end_col - start_col + 1), columns=variables_to_extract)

    # Standardize the text by converting to lower case and stripping extra spaces
    data_r = data.applymap(lambda x: x.strip().lower() if isinstance(x, str) else x)

    # Extract data for these variables, maintaining row order
    for index, row in data_r.iterrows():
        for col in range(5, 9):  # Check columns 5 to 8 for variable names
            if row[col] in variables_to_extract:
                values = row[start_col:end_col + 1].tolist()
                extracted_data.loc[:, row[col]] = values

    # Rename columns based on the variable dictionary
    extracted_data.rename(columns=var_dict, inplace=True)

    # Fill missing columns with NaN
    for var in var_dict.values():
        if var not in extracted_data.columns:
            extracted_data[var] = None

    # Convert negative numbers to positive, handling null values
    for col in ['t_staff_costs', 'contract_staff_costs', 'direct_costs', 'other_direct_costs', 'cost/hd', 'overheads', 'expenses']:
        if col in extracted_data.columns:
            try:
                extracted_data[col] = -extracted_data[col].astype(float)
            except Exception:
                pass


    return extracted_data

