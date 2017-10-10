# Do float devision by default.
from __future__ import division, print_function

import boto3
import numpy as np
import sys
import math
import re

from datetime import datetime, date, time, timedelta

# Get data from all m4/c4 instances
mc_instances = ['m4.large','m4.xlarge','m4.2xlarge','m4.4xlarge','m4.10xlarge',
            'c4.large','c4.xlarge','c4.2xlarge','c4.4xlarge','c4.8xlarge']

# Get data from all g2 instances
gpu_instances = ['g2.2xlarge', 'g2.8xlarge']

all_instances = mc_instances + gpu_instances

# all regions under investigation
regions = ['us-east-1', 'us-west-2', 'us-west-1' , 'eu-west-1', 'eu-central-1',
           'ap-southeast-1', 'ap-northeast-1', 'ap-southeast-2',
           'ap-northeast-2', 'ap-south-1', 'sa-east-1']

# Number of hours in a week
hours = 7 * 24

# Start on Oct 23rd
# (starts at 0:00 by default)
start_date = datetime(2016, 10, 23) # CHANGE THIS

print("hour instance region zone price")

for region in regions:

    # Need a new client for each region.
    client = boto3.client("ec2", region)

    # For each hour since midnight on the first day, 
    ## get data for the next hour.
    for hour in range(0, hours):
        deltaA = timedelta(hours=hour)
        deltaB = timedelta(hours=(hour + 1))

        start = start_date + deltaA
        end = start_date + deltaB

        # Query boto client
        response = client.describe_spot_price_history(
            InstanceTypes = all_instances,
            StartTime = start,
            EndTime = end,
            ProductDescriptions = ["Linux/UNIX"])

        # Extract identifying information from boto response and print.
        for item in response["SpotPriceHistory"]:
            print(' '.join([str(hour),
                item["InstanceType"],
                region,
                item["AvailabilityZone"], 
                str(item["SpotPrice"])]))
