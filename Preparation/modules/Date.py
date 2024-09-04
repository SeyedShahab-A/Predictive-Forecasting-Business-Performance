#Date.py
import pandas as pd
from datetime import datetime

monthsabv = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]

def find_starting_point(data):

    for row in range(15):
        for col in range(data.shape[1]):
            cell_value = str(data.iloc[row, col]).strip().lower()
            if cell_value in monthsabv:
                if row + 1 < data.shape[0]:
                    next_cell_value = str(data.iloc[row + 1, col]).strip()
                    if '/' in next_cell_value and len(next_cell_value) == 7:
                        try:
                            year_parts = next_cell_value.split('/')
                            if len(year_parts) == 2 and all(part.isdigit() for part in year_parts):
                                start_year = int(year_parts[0])
                                end_year = int(year_parts[1])
                                if end_year == (start_year % 100) + 1:  # Check if the year structure is valid
                                    return cell_value.capitalize(), f"{start_year}/{end_year}", row, col
                        except:
                            continue
    return None, None, None, None


def find_specific_date(data, target_month, target_year):
    target_month = target_month.strip().lower()
    target_year = target_year.strip()
    for row in range(15):
        for col in range(data.shape[1]):
            cell_value = str(data.iloc[row, col]).strip().lower()
            if cell_value == target_month:
                if row + 1 < data.shape[0]:
                    next_cell_value = str(data.iloc[row + 1, col]).strip()
                    if next_cell_value == target_year:
                        return row, col
    return None, None


# Function to convert financial year and month to real date
def financial_year_to_date(year_str, month_str):
    # Convert month abbreviation to month number
    month = datetime.strptime(month_str, "%b").month

    # Extract the start year of the financial year
    start_year = int(year_str.split('/')[0])

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

    # Filter out invalid month entries
    valid_months = [month for month in months if isinstance(month, str) and month in monthsabv]
    valid_years = [year for year in financial_years if isinstance(year, str) and '/' in year]

    # Generate list of dates
    dates = [financial_year_to_date(year, month) for year, month in zip(valid_years, valid_months)]
    dates = pd.to_datetime(dates)

    return dates