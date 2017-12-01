#
# TODO:
#   - Add better error handling
#   - Check endpoint format
#

import argparse
import logging
from web3 import Web3, HTTPProvider
from time import sleep
from sys import exit

default_txs_generation_interval = 2000
default_endpoint = 'http://localhost:8546'
default_logging_level = logging.DEBUG
default_account_passphrase = '1234'
default_tx_value = 0
default_tx_gas_price = 0
default_gas_limit = 21000                   # Set equal to the instrinsic gas

def fatal(error_message, exit_value = -1):

    logging.error(error_message)
    exit(exit_value)


def parse_args():

    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--interval', type = int, help = 'TXs generation interval in milliseconds')
    parser.add_argument('-e', '--endpoint', help = 'Endpoint used for connection in the format http://<ip>[:<port>]')
    args = parser.parse_args()

    interval = args.interval
    if not interval:
        interval = default_txs_generation_interval

    endpoint = args.endpoint
    if not endpoint:
        endpoint = default_endpoint

    logging.info('Generating TXs every {} ms. for endpoint {}'.format(interval, endpoint))

    return interval, endpoint


def create_account():

    account_address = web3.personal.newAccount(default_account_passphrase)
    logging.info('Account {} successfully created'.format(account_address))

    is_unlocked = web3.personal.unlockAccount(account_address, default_account_passphrase, "0")
    if is_unlocked:
        logging.info('Account {} successfully unlocked'.format(account_address))
    else:
        fatal('Cannot unlock account {}, exiting'.format(account_address))

    return account_address


def send_transaction(sender_address, recipient_address, value, sleep_time):

    tx_id = web3.eth.sendTransaction({
        'to': recipient_address,
        'from': sender_address,
        'gasPrice': default_tx_gas_price,
        'value': value,
        'gas': default_gas_limit})

    logging.debug('Sent TX with ID {} from {} to {} with value {}'
        .format(tx_id, sender_address, recipient_address, value))

    sleep(sleep_time)

    return


def ping_pong(sleep_time, account_address1, account_address2):

    send_transaction(account_address1, account_address2, default_tx_value, sleep_time)
    send_transaction(account_address2, account_address1, default_tx_value, sleep_time)

    return


#
# Main
#

logging.basicConfig(format = '%(asctime)s - %(levelname)s - %(message)s', level = default_logging_level)

txs_generation_interval, txs_generation_endpoint = parse_args()

web3 = Web3(HTTPProvider(txs_generation_endpoint))

account_address1 = create_account()
account_address2 = create_account()

sleep_time = txs_generation_interval / 1000.0
while True:
    ping_pong(sleep_time, account_address1, account_address2)
