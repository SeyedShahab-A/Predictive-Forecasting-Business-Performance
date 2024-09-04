import pandas as pd
from datetime import datetime

# Define month abbreviations and full names

# Define month abbreviations, full names, and periods
months_abbr = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]
months_full = ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"]
periods = [f'p{i}' for i in range(1, 13)]

# Create the mapping from abbreviated month names to full month names
month_mapping = {abbr: full for abbr, full in zip(months_abbr, months_full)}

# Create the mapping from periods P1 to P12 starting from May
ordered_months_full = months_full[4:] + months_full[:4]
period_mapping = {period: full for period, full in zip(periods, ordered_months_full)}
full_mapping = {full:full for full in months_full}
month_mapping.update(period_mapping)
month_mapping.update(full_mapping)

def find_starting_point(data):
    for row in range(15):
        for col in range(data.shape[1]):
            cell_value = str(data.iloc[row, col]).strip().lower()
            if cell_value in months_abbr or cell_value in month_mapping:
                if cell_value in month_mapping:
                    cell_value = month_mapping[cell_value]  # Map to abbreviation
                if row + 1 < data.shape[0]:
                    next_cell_value = str(data.iloc[row + 1, col]).strip()
                    if '/' in next_cell_value:
                        try:
                            year_parts = next_cell_value.split('/')
                            if len(year_parts) == 2:
                                start_year, end_year = year_parts
                                if len(start_year) == 4:  # Format yyyy/yy
                                    start_year = int(start_year)
                                    if len(end_year) == 4:
                                        end_year = int(end_year)
                                    elif len(end_year) == 2:
                                        end_year = int('20' + end_year)
                                elif len(start_year) == 2:  # Format yy/yy
                                    start_year = int('20' + start_year)
                                    end_year = int('20' + end_year)
                                if end_year == start_year + 1:  # Check if the year structure is valid
                                    return cell_value.capitalize(), f"{start_year}/{end_year}", row, col
                        except Exception as e:
                            continue
    return None, None, None, None


def normalize_year_format(year_str):
    parts = year_str.split('/')
    if len(parts) == 2:
        if len(parts[0]) == 4 and len(parts[1]) == 4:
            return f"{parts[0]}/{parts[1]}"
        elif len(parts[0]) == 4 and len(parts[1]) == 2:
            return f"{parts[0]}/20{parts[1]}"
        elif len(parts[0]) == 2 and len(parts[1]) == 2:
            return f"20{parts[0]}/20{parts[1]}"
    return year_str  # Return the original if it doesn't match known formats


def find_specific_date(data, target_month, target_year):
    target_month = target_month.strip().lower()
    target_year = normalize_year_format(target_year.strip())

    # Map the target month to its full name if needed
    if target_month in month_mapping:
        target_month = month_mapping[target_month]

    for row in range(15):
        for col in range(data.shape[1]):
            cell_value = str(data.iloc[row, col]).strip().lower()
            if cell_value in month_mapping:
                cell_value = month_mapping[cell_value]  # Map to full month name
            if cell_value == target_month:
                if row + 1 < data.shape[0]:
                    next_cell_value = str(data.iloc[row + 1, col]).strip()
                    normalized_next_cell_value = normalize_year_format(next_cell_value)
                    if normalized_next_cell_value == target_year:
                        return row, col
    return None, None

# Function to convert financial year and month to real date
def financial_year_to_date(year_str, month_str):
    # Handle periods (P1 to P12)
    month_str = month_str.strip().lower()
    month = month_mapping[month_str]
    month = datetime.strptime(month, "%B").month

    # Extract the start year of the financial year
    start_year = int(year_str.split('/')[0])
    if start_year < 100:  # Correct for 2-digit years
        start_year += 2000

    # Determine the actual year for the date
    if month >= 5:  # May to December
        year = start_year
    else:  # January to April
        year = start_year + 1

    return datetime(year, month, 1)

def convert_date(data, row, start_col, end_col):
    months = data.iloc[row, start_col:end_col+1].values.flatten().tolist()  # Months
    months = [month.strip().lower() for month in months]
    financial_years = data.iloc[row+1, start_col:end_col+1].values.flatten().tolist()  # Financial years

    # Map months to full names
    months = [month_mapping.get(month, month) for month in months]

    # Filter out invalid month entries
    valid_months = [month for month in months if isinstance(month, str) and month in months_full]
    valid_years = [year for year in financial_years if isinstance(year, str) and '/' in year]

    # Generate list of dates
    dates = [financial_year_to_date(year, month) for year, month in zip(valid_years, valid_months)]
    dates = pd.to_datetime(dates)

    return dates
