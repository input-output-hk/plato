#
# TODO:
#   - Add better error handling
#

import argparse
import logging
from web3 import Web3, HTTPProvider
from time import sleep
from sys import exit


default_txs_generation_interval = 2000
default_logging_level = logging.DEBUG
default_account_passphrase = '1234'
default_tx_value = 0
default_tx_gas_price = 0


def fatal(error_message, exit_value = -1):

    logging.error(error_message)
    exit(exit_value)


def parse_args():

    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--interval', type = int, help = 'TXs generation interval in milliseconds')
    args = parser.parse_args()

    interval = args.interval
    if not interval:
        interval = default_txs_generation_interval

    logging.info('Generating TXs every {} ms.'.format(interval))

    return interval


def create_account():

    account_address = web3.personal.newAccount(default_account_passphrase)
    logging.info('Account {} successfully created'.format(account_address))

    is_unlocked = web3.personal.unlockAccount(account_address, default_account_passphrase)
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
        'value': value})

    logging.debug('Sent TX with ID {} from {} to {} with value {}'
        .format(tx_id.hex(), sender_address, recipient_address, value))

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

txs_generation_interval = parse_args()

web3 = Web3(HTTPProvider('http://localhost:8546'))

account_address1 = create_account()
account_address2 = create_account()

# NOTE: For testing purposes only:
#account_address1 = '0xC9851452EC56d4B60ae8B09c1E2dC3537f1D8015'
#account_address2 = '0x8E6516f2e9da8E36bd3aa62BDa457826BD88064b'
#web3.personal.unlockAccount(account_address1, default_account_passphrase)
#web3.personal.unlockAccount(account_address2, default_account_passphrase)

sleep_time = txs_generation_interval / 1000.0
while True:
    ping_pong(sleep_time, account_address1, account_address2)
