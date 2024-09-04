#Data_Quality.py
exclude_column = ['Division', 'probable', 'possible', 'speculative']

columns_to_keep = [
    'Date','secured', 'direct_costs', 'overheads',
    'total_headcount', 'Region', 'Cost_Centre', 'Division'
]


def check_missing_values(df, columns_to_keep):
    # Create a boolean mask that ignores the specified column
    mask = df[columns_to_keep].isnull()

    # Check if there are any missing values in the mask
    has_missing = mask.any().any()
    rows_with_missing = df[mask.any(axis=1)]

    # Get the rows that have missing values

    return has_missing, rows_with_missing

def check_missing_secured(df):
    return df['secured'].isnull().any()


def check_duplicates(df, columns_to_keep):
    # Remove the date column
    df_custom = df[columns_to_keep]

    # Check for duplicate rows
    duplicate_rows = df_custom[df_custom.duplicated()]

    # Check for duplicate columns
    duplicate_columns = df_custom.T[df_custom.T.duplicated()].T

    # Return results
    return not duplicate_rows.empty or not duplicate_columns.empty



def rond(series):
    # Function to round each value in the series to 4 decimal places
    return series.apply(lambda x: round(x, 4))

def check_relationships(df):
    # Check gross_profit relationship
    if all(col in df.columns for col in ['secured', 'probable', 'possible', 'speculative', 'biso', 'direct_costs', 'gross_profit']):
        expected_gross_profit = df[['secured', 'probable', 'possible', 'speculative']].sum(axis=1, skipna=True) + df['biso'] - df['direct_costs']
        #df['net'] = expected_gross_profit - df['gross_profit']
        if not (rond(df['gross_profit'] - expected_gross_profit)==0).all():
            return True

    # Check net_profit relationship
    if all(col in df.columns for col in ['gross_profit', 'overheads', 'net_profit', 'subcon']):
        expected_net_profit = df['gross_profit'] - df['overheads'] + df['subcon']
        #df['net_prof'] = expected_net_profit - df['net_profit']
        if not (rond(df['net_profit'] - expected_net_profit)==0).all():
            return True

    # Check total_headcount relationship
    if all(col in df.columns for col in ['t_technical_staff', 'contract_staff', 'total_headcount']):
        expected_total_headcount = df['t_technical_staff'] + df['contract_staff']
        #df['net_head'] = df['total_headcount'] - expected_total_headcount
        if not (rond(df['total_headcount'] - expected_total_headcount)==0).all():
            return True

    # Check direct_costs relationship
    if all(col in df.columns for col in ['direct_costs', 'other_direct_costs', 'biso']):
        expected_direct_costs = df['other_direct_costs'] + df['biso']
        if not (rond(df['direct_costs'] - expected_direct_costs)==0).all():
            return True

    # Check other_direct_costs relationship
    if all(col in df.columns for col in ['t_staff_costs', 'other_direct_costs', 'contract_staff_costs', 'expenses']):
        expected_other_direct_costs = df['contract_staff_costs'] + df['t_staff_costs'] + df['expenses']
        if not (rond(df['other_direct_costs'] - expected_other_direct_costs)==0).all():
            return True

    return False
