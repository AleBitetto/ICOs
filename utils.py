import os
import zipfile
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import shutil
import time
from timeit import default_timer as timer
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
import pandas as pd
import numpy as np
import requests
import joblib
from bs4 import BeautifulSoup
from soup2dict import convert
import datetime
import re
import logging
from pdf2image import convert_from_path
import pytesseract
from PIL import Image
from tika import parser
from thefuzz import process
import json
from urllib.parse import urljoin
import torch
from transformers import BertTokenizer, BertForSequenceClassification
from termcolor import colored



def get_chromedriver(chromedriver_path=None, use_proxy=False, user_agent=None,
                    PROXY_HOST=None, PROXY_PORT=None, PROXY_USER=None, PROXY_PASS=None, download_folder=None,
                    desired_capabilities=None):

    manifest_json = """
    {
        "version": "1.0.0",
        "manifest_version": 2,
        "name": "Chrome Proxy",
        "permissions": [
            "proxy",
            "tabs",
            "unlimitedStorage",
            "storage",
            "<all_urls>",
            "webRequest",
            "webRequestBlocking"
        ],
        "background": {
            "scripts": ["background.js"]
        },
        "minimum_chrome_version":"22.0.0"
    }
    """

    background_js = """
    var config = {
            mode: "fixed_servers",
            rules: {
            singleProxy: {
                scheme: "http",
                host: "%s",
                port: parseInt(%s)
            },
            bypassList: ["localhost"]
            }
        };

    chrome.proxy.settings.set({value: config, scope: "regular"}, function() {});

    function callbackFn(details) {
        return {
            authCredentials: {
                username: "%s",
                password: "%s"
            }
        };
    }

    chrome.webRequest.onAuthRequired.addListener(
                callbackFn,
                {urls: ["<all_urls>"]},
                ['blocking']
    );
    """ % (PROXY_HOST, PROXY_PORT, PROXY_USER, PROXY_PASS)

    chrome_options = webdriver.ChromeOptions()
    # allow multiple download
    prefs_experim = {'profile.default_content_setting_values.automatic_downloads': 1}
    
    if use_proxy:
        pluginfile = 'proxy_auth_plugin.zip'

        with zipfile.ZipFile(pluginfile, 'w') as zp:
            zp.writestr("manifest.json", manifest_json)
            zp.writestr("background.js", background_js)
        chrome_options.add_extension(pluginfile)
    if user_agent:
        chrome_options.add_argument('--user-agent=%s' % user_agent)
    if download_folder:
        prefs_experim["download.default_directory"] = download_folder

    chrome_options.add_experimental_option("prefs", prefs_experim)
    driver = webdriver.Chrome(
        executable_path=chromedriver_path,
        chrome_options=chrome_options,
        desired_capabilities=desired_capabilities)
    return driver


def pdf_to_text(file_path='', tesseract_path='', lang='eng'):
    
    # open pdf
    parsed_pdf = parser.from_file(file_path)

    # extract text and metadata
    txt = parsed_pdf['content']
    meta = parsed_pdf['metadata']

    # if txt is None try to parse pdf as image and extract text with OCR
    # https://softhints.com/python-extract-text-from-image-or-pdf/
    # https://towardsdatascience.com/extracting-text-from-scanned-pdf-using-pytesseract-open-cv-cd670ee38052
    # install tesseract https://linuxhint.com/install-tesseract-windows/
    if txt is None:
        try:
            pytesseract.pytesseract.tesseract_cmd=tesseract_path

            # convert pages to images
            pages = convert_from_path(file_path, 350)

            # extract text
            txt=''
            for pag in pages:
                txt=txt+'\n'+pytesseract.image_to_string(pag, lang = lang)
        except:
            pass
    
    return txt, meta, parsed_pdf


def download_from_drive_dropbox(chromedriver_path='', download_url='', download_folder='', temp_folder='', pdf_name='',
                                move_folder='', source=''):
    
    # create folder
    if os.path.exists(temp_folder):
        shutil.rmtree(temp_folder)

    os.makedirs(temp_folder)
    
    # download file
    driver = get_chromedriver(chromedriver_path = chromedriver_path, download_folder=download_folder)
    driver.get(download_url)
    if source == 'drive':
        download_button_xpath = ["/html/body/div[3]/div[4]/div/div[3]/div[2]/div[2]/div[3]"]
    elif source == 'dropbox':
        download_button_xpath = ["/html/body/div[3]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div[1]/button/span",
                                 "/html/body/div[1]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div[1]/span/button/span",
                                 "/html/body/div[3]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div/span/button"]                                
        time.sleep(2)
        ActionChains(driver).send_keys(Keys.ESCAPE).perform()
    else:
        raise ValueError('Please provide "source", can be "drive" or "dropbox"')
    page_error = "page not available"
    try:
        time.sleep(1.5)
        driver.find_element(By.CLASS_NAME, "action-bar-action-DOWNLOAD_ACTION").click()
        page_error = ""
    except:
        pass
    try:
        time.sleep(1.5)
        driver.find_element("xpath", download_button_xpath[0]).click()
        page_error = ""
    except:
        pass
    try:
        time.sleep(1.5)
        driver.find_element("xpath", download_button_xpath[1]).click()
        page_error = ""
    except:
        pass
    try:
        time.sleep(1.5)
        driver.find_element("xpath", download_button_xpath[2]).click()
        page_error = ""
    except:
        pass
    if page_error != "":
        return page_error
    
    # wait for download
    end_time = time.time() + 30
    while not any(x.endswith('.pdf') for x in os.listdir(temp_folder)):
        time.sleep(1)
        if time.time() > end_time:
            return "out of time"
    time.sleep(1)
    driver.close()
    
    # rename and move
    file_list = os.listdir(temp_folder)
    if len(file_list) > 0:
        os.rename(os.path.join(temp_folder, file_list[0]), os.path.join(temp_folder, pdf_name))
        shutil.move(os.path.join(temp_folder, pdf_name), os.path.join(move_folder, pdf_name))
        try:
            shutil.rmtree(temp_folder)
        except:
            pass
        return "ok"
    else:
        try:
            shutil.rmtree(temp_folder)
        except:
            pass
        return "file not found"


def adjust_date(x):
    # cut Month name to 3 char only
    x=x.split(' ')
    if len(x) > 1:
        x[1]=x[1][:3]
        x=' '.join(x)
    else:
        x=x[0]
        
    return x


def eval_duration(x):
    
    duration=None
    if x['StartDate'] not in ['TBA', '']:
        try:
            max_date=pd.to_datetime(x['ListDownloadedOn'], format="%d/%m/%Y")
            start=pd.to_datetime(x['StartDate'])
            end=pd.to_datetime(x['EndDate']) if x['EndDate'] not in ['TBA', ''] else pd.to_datetime('1 Jan 2150')
            duration=min([end, max_date])-start
            if duration.days < 0:
                duration=-99
            else:
                duration=np.log10(duration.days + 1)   # avoid log(0)
        except:
            duration=-99
            print(f'#### wrong date on index {x.name}: {x.StartDate} or {x.EndDate}')
    
    return(duration)


def get_social_series(url='', tot_series=1, chromedriver_path=''):
    
    # https://levelup.gitconnected.com/trickycases-6-scrape-highcharts-plots-a6b3fc233fe6
    
    driver = get_chromedriver(chromedriver_path=chromedriver_path)
    driver.get(url)
    series_dict={}
    try:
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, "highcharts-container ")))

        for i in range(tot_series):            
            cmd='return Highcharts.charts[0].series['+str(i)+']'
            series_name=driver.execute_script(cmd+'.name')
            dates = driver.execute_script(cmd+'.data.map(x => x.series).map(x => x.xData)[0].map(x => new Date(x).toISOString())')
            values = driver.execute_script(cmd+'.data.map(x => x.series).map(x => x.yData)[0]')

            series_dict[series_name]=pd.DataFrame({'Date': dates, 'Users': values})
    except:
        pass

    driver.close()

    return series_dict


def scrape_info_icomarks(url='', chromedriver_path='', skip_social=False):
    
    '''
    - skip_social: if True skip social users' timeseries download (takes time and uses WebDriver)
    '''
    
    add_row=pd.DataFrame()
    
    #### request page
    page = requests.get(url)
    soup = BeautifulSoup(page.content, 'html.parser')


    #### page screenshot date
    add_row['url']=[url]
    try:
        tag = soup.find_all('div', class_="swimm-panel-bottom__links", recursive=True)
        conv_dict = convert(tag)
        add_row['PageScreenshot']=[conv_dict['div'][0]['#text']]
    except:
        pass


    #### extract rating

    tag = soup.find_all('div', class_="ico-rating-content", recursive=True)
    conv_dict = convert(tag)
    if len(conv_dict) > 0:
        # overall rating
        ind=np.where([x['@class'] == ['ico-rating-overall'] for x in conv_dict['div'][0]['div']])[0][0]
        value=conv_dict['div'][0]['div'][ind]['#text']   
        add_row['Rating_TOTAL_SCORE']=[value]
        # rating component
        for t in conv_dict['div'][0]['div'][2]['div']:
            if '@class' in t.keys():
                if t['@class'] == ['ico-rating__item']:
                    ind=np.where([x['@class'] == ['ico-rating__title'] for x in t['div']])[0][0]
                    name=t['div'][ind]['#text']
                    name=re.sub('ICO |STO |IEO ', '', name)
                    ind=np.where([x['@class'] == ['ico-rating__circle'] for x in t['div']])[0][0]
                    value=t['div'][ind]['#text']
                    add_row['Rating_'+name.replace(' ', '_')]=[value]



    #### extract "Detail" tab blocks

    tag = soup.find_all('div', class_="icoinfo-block", recursive=True)
    conv_dict = convert(tag)

    block_log={}
    block_df=pd.DataFrame()
    for t in conv_dict['div']:

        # block name
        ind=np.where([x['@class'] == ['icoinfo-block__title'] for x in t['div']])[0][0]
        block_name=t['div'][ind]['#text']

        # extract block contents
        try:
            ind=np.where([x['@class'] == ['icoinfo-block-content'] for x in t['div']])[0][0]
            block_log[block_name]=t['div'][ind]['#text']   # save log of displayed items
            block_dict=t['div'][ind]['div']
        except:   # means tab of social media
            block_dict=t['div']

        block_t=pd.DataFrame()
        for t1 in block_dict:
            if t1['@class'] == ['icoinfo-block__item']:
                item_name=t1['span'][0]['#text'].replace(':', '')
                item_url=None
                if 'a' in t1.keys():
                    if '@href' in t1['a'][0]:
                        item_url=t1['a'][0]['@href']                
                    item_display=t1['a'][0]['navigablestring']
                elif 'i' in t1.keys():
                    if 'navigablestring' in t1['i'][0]:
                        item_display=t1['i'][0]['navigablestring']
                else:
                    try:
                        item_display=t1['navigablestring']
                    except:
                        item_display=None
                if type(item_display) == list:
                    if len(item_display) > 1:
                        item_display='; '.join(item_display)
                    else:
                        item_display=item_display[0]
                block_t=pd.concat([block_t, pd.DataFrame({'BlockName': block_name,
                                                         'Item': item_name,
                                                         'ItemUrl': item_url,
                                                         'ItemValue': item_display}, index=[0])])

        block_df=pd.concat([block_df, block_t])
    add_row['InfoBlock']=[block_df]


    #### Get team size and members
    
    try:
        tag = soup.find_all('a', href=True, recursive=True)
        conv_dict = convert(tag)
        ind=np.where([x['@href'] == '#team' for x in conv_dict['a']])[0][0]
        team_size=int(conv_dict['a'][ind]['#text'].replace('Team (', '').replace(')', ''))
        # check if Advisors
        advisor_size=int(soup.findAll(text = re.compile('Advisors \('))[0].replace('Advisors (', '').replace(')', ''))
        loop_max = 2 if advisor_size != 0 else 1
        # extract Team and Advisors
        tag = soup.find_all('div', class_='company-team', recursive=True)
        conv_dict = convert(tag)
        team_df=pd.DataFrame()
        for team_ind in range(loop_max):    # 0 = Team  1 = Advisor
            team_lab = 'Team' if team_ind == 0 else 'Advisor'
            for t in conv_dict['div'][team_ind]['div']:
                person_name=t['div'][0]['#text']
                person_role=t['div'][1]['#text']
                person_extra=None
                if t['div'][2]['@class'] == ['company-team__post']:
                    person_extra=t['div'][2]['#text']
                if ['company-team__links'] in [x['@class'] for x in t['div']]:
                    ind=np.where([x['@class'] == ['company-team__links'] for x in t['div']])[0][0]
                    person_link=[x['@href'] for x in t['div'][ind]['a']]
                else:
                    person_link=None
                team_df=pd.concat([team_df, pd.DataFrame({'Member': team_lab,
                                                          'Name': person_name,
                                                          'Role': person_role,
                                                          'Extra': person_extra,
                                                          'Links': [person_link]}, index=[0])])
        if team_size != team_df[team_df['Member']=='Team'].shape[0]:
            print(f"- {url} mismatch between 'TeamSize' and extracted team members")
        if advisor_size != team_df[team_df['Member']=='Advisor'].shape[0]:
            print(f"- {url} mismatch between 'AdvisorSize' and extracted advisor members")
        add_row['TeamSize']=team_size
        add_row['AdvisorSize']=advisor_size
        add_row['TeamBlock']=[team_df]
    except:
        pass


    #### Get Social Rating and users timeseries

    tag = soup.find_all('div', class_='companyTab companyTab_social', recursive=True)
    conv_dict = convert(tag)

    if len(conv_dict) > 0:
        social_df=pd.DataFrame()
        try:
            for t in conv_dict['div'][0]['div'][0]['div']:
                if t['@class'] == ['social-item']:
                    social_name=t['div'][0]['div'][0]['#text']
                    total_user=int(t['div'][1]['div'][2]['#text'].replace(',', ''))
                    rating=t['div'][1]['div'][3]['#text']
                    social_df=pd.concat([social_df, pd.DataFrame({'Social': social_name,
                                                                 'Users': total_user,
                                                                 'Rating': rating}, index=[0])])
        except:
            pass
        
        # download chart data
        if not skip_social:
            series_dict=get_social_series(url=url, tot_series=social_df.shape[0], chromedriver_path=chromedriver_path)
            series_status = 'DOWNLOADED' if len(series_dict) != 0 else 'DOWNLOAD_ERROR'
        else:
            series_dict={}
            series_status='DOWNLOAD_SKIPPED'

        add_row['SocialWithRating']=social_df.shape[0]
        add_row['SocialSeriesStatus']=series_status
        add_row['SocialBlock']=[[{'stats': social_df, 'timeseries': series_dict}]]
        
    return add_row


def extract_scaping_icomarks(scrape_df):
    
    df_final=pd.DataFrame()    
    for index, row in scrape_df.iterrows():

        print(f'   - Processing {str(index + 1)} / {len(scrape_df)}', end='\r')

        if row['ScrapeStatus'] != 'ERROR':

            url=row['url']
            add_row=pd.DataFrame({'url': url}, index=[0])
            block_df=row['InfoBlock']

            #### unpack Icomarks rating

            add_row['RatingIcomarks']=row['Rating_TOTAL_SCORE']


            #### unpack Team size

            add_row['TeamSize']=row['TeamSize']
            add_row['AdvisorSize']=row['AdvisorSize']


            #### unpack General tab

            blk_gen=block_df[block_df['BlockName'] == 'General']
            blk_gen_item=blk_gen['Item'].values
            # bounty - dummy
            bounty= 1 if 'Bounty' in blk_gen_item else 0
            # MVP - dummy
            mvp= 1 if 'MVP' in blk_gen_item else 0
            # country
            country= blk_gen[blk_gen['Item'] == 'Country']['ItemValue'].values if 'Country' in blk_gen_item else ''
            # dates (usually is just TBA)
            dates=blk_gen[blk_gen['Item'] == 'Dates']['ItemValue'].values if 'Dates' in blk_gen_item else ''
            # ICO dates
            if 'ICO Time' in blk_gen_item:
                dt=blk_gen[blk_gen['Item'] == 'ICO Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    ico_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    ico_date_start=dt[0]
                try:
                    ico_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    ico_date_end=dt[1]
            else:
                ico_date_start=None
                ico_date_end=None
            # IEO dates
            if 'IEO Time' in blk_gen_item:
                dt=blk_gen[blk_gen['Item'] == 'IEO Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    ieo_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    ieo_date_start=dt[0]
                try:
                    ieo_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    ieo_date_end=dt[1]
            else:
                ieo_date_start=None
                ieo_date_end=None
            # IEO launchpad
            ieo_launch= blk_gen[blk_gen['Item'] == 'IEO launchpad']['ItemValue'].values if 'IEO launchpad' in blk_gen_item else ''
            # STO dates
            if 'STO Time' in blk_gen_item:
                dt=blk_gen[blk_gen['Item'] == 'STO Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    sto_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    sto_date_start=dt[0]
                try:
                    sto_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    sto_date_end=dt[1]
            else:
                sto_date_start=None
                sto_date_end=None
            # Pre-sale Time dates and dummy
            if 'Pre-sale Time' in blk_gen_item:
                pres_dummy=1
                dt=blk_gen[blk_gen['Item'] == 'Pre-sale Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    pres_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    pres_date_start=dt[0]
                try:
                    pres_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    pres_date_end=dt[1]
            else:
                pres_date_start=None
                pres_date_end=None
                pres_dummy=0
            # Website and dummy
            if 'Website' in blk_gen_item:
                website=blk_gen[blk_gen['Item'] == 'Website']['ItemUrl'].values
                website_dummy=1
            else:
                website=''
                website_dummy=0
            # Whitepaper and dummy
            if 'White paper' in blk_gen_item:
                whitepaper=blk_gen[blk_gen['Item'] == 'White paper']['ItemUrl'].values
                whitepaper_dummy=1
            else:
                whitepaper=''
                whitepaper_dummy=0
            # Whitelist/KYC
            if 'Whitelist/KYC' in blk_gen_item:
                if blk_gen[blk_gen['Item'] == 'Whitelist/KYC']['ItemValue'].values in ['KYC', 'Whitelist + KYC']:
                    kyc= 1 
                if blk_gen[blk_gen['Item'] == 'Whitelist/KYC']['ItemValue'].values in ['Whitelist', 'Whitelist + KYC']:
                    whitelist= 1
                if blk_gen[blk_gen['Item'] == 'Whitelist/KYC']['ItemValue'].values not in ['Whitelist', 'KYC', 'Whitelist + KYC']:
                    print(f'- unexpected value in "Whitelist/KYC" for {url}')
            else:
                kyc=0
                whitelist=0

            add_row['BountyDummy']=bounty
            add_row['MVPDummy']=mvp
            add_row['Country']=country
            add_row['Dates']=dates
            add_row['ICODateStart']=ico_date_start
            add_row['ICODateEnd']=ico_date_end
            add_row['IEODateStart']=ieo_date_start
            add_row['IEODateEnd']=ieo_date_end
            add_row['IEOLaunchpad']=ieo_launch
            add_row['STODateStart']=sto_date_start
            add_row['STODateEnd']=sto_date_end
            add_row['PreSaleDummy']=pres_dummy
            add_row['PreSaleDateStart']=pres_date_start
            add_row['PreSaleDateEnd']=pres_date_end
            add_row['WebsiteDummy']=website_dummy
            add_row['WebsiteUrl']=website
            add_row['WhitepaperDummy']=whitepaper_dummy
            add_row['WhitepaperUrl']=whitepaper
            add_row['KYCDummy']=kyc
            add_row['WhitelistDummy']=whitelist


            #### unpack Token info tab

            blk_tok=block_df[block_df['BlockName'] == 'Token info']
            blk_tok_item=blk_tok['Item'].values

            # ticker
            ticker= blk_tok[blk_tok['Item'] == 'Ticker']['ItemValue'].values if 'Ticker' in blk_tok_item else ''
            # platform
            platform= blk_tok[blk_tok['Item'] == 'Platform']['ItemValue'].values if 'Platform' in blk_tok_item else ''
            # Token Type and ERC20 - dummy
            toktype= blk_tok[blk_tok['Item'] == 'Token Type']['ItemValue'].values if 'Token Type' in blk_tok_item else ['']
            erc20= 1 if 'erc' in toktype[0].lower() else 0
            # available for sale
            avail_sale= blk_tok[blk_tok['Item'] == 'Available for sale']['ItemValue'].values if 'Available for sale' in blk_tok_item else ''
            # total supply
            tot_supply= blk_tok[blk_tok['Item'] == 'Total supply']['ItemValue'].values if 'Total supply' in blk_tok_item else ''        

            add_row['Ticker']=ticker
            add_row['Platform']=platform
            add_row['TokenType']=toktype
            add_row['ERC20Dummy']=erc20
            add_row['TokenAvailForSale']=avail_sale
            add_row['TokenTotSupply']=tot_supply


            #### unpack Financial tab

            blk_fin=block_df[block_df['BlockName'] == 'Financial']
            blk_fin_item=blk_fin['Item'].values

            # raised
            raised= blk_fin[blk_fin['Item'] == 'Raised']['ItemValue'].values if 'Raised' in blk_fin_item else ''
            # Hard/Soft cap
            hardcap= blk_fin[blk_fin['Item'] == 'Hard cap']['ItemValue'].values if 'Hard cap' in blk_fin_item else ''
            softcap= blk_fin[blk_fin['Item'] == 'Soft cap']['ItemValue'].values if 'Soft cap' in blk_fin_item else ''
            # accepted currency
            accepted= blk_fin[blk_fin['Item'] == 'Accepting']['ItemValue'].values if 'Accepting' in blk_fin_item else ''
            # bonuses dummy
            bonus= 1 if 'Bonuses' in blk_fin_item else 0
            # ICO, IEO, STO, Pre-Sale price
            ico_price= blk_fin[blk_fin['Item'] == 'ICO Price']['ItemValue'].values if 'ICO Price' in blk_fin_item else ''
            ieo_price= blk_fin[blk_fin['Item'] == 'IEO Price']['ItemValue'].values if 'IEO Price' in blk_fin_item else ''
            sto_price= blk_fin[blk_fin['Item'] == 'STO Price']['ItemValue'].values if 'STO Price' in blk_fin_item else ''
            pres_price= blk_fin[blk_fin['Item'] == 'Pre-sale Price ']['ItemValue'].values if 'Pre-sale Price ' in blk_fin_item else ''

            add_row['FundRaised']=raised
            add_row['FundHardCap']=hardcap
            add_row['FundSoftCap']=softcap
            add_row['AcceptedCurr']=accepted
            add_row['BonusDummy']=bonus
            add_row['ICOPrice']=ico_price
            add_row['IEOPrice']=ieo_price
            add_row['STOPrice']=sto_price
            add_row['PreSalePrice']=pres_price


            #### unpack Social Media tab

            blk_soc=block_df[block_df['BlockName'] == 'Social media']
            media_list=None
            if len(blk_soc) > 0:
                media_list=[blk_soc['ItemValue'].values.tolist()]

            add_row['SocialMedia']=media_list


            #### unpack Social Stats

    #         if not np.isnan(row['SocialWithRating']):

    #             track_social=row['SocialWithRating']

    #             if row['SocialSeriesStatus'] == 'DOWNLOADED':

    #                 blk_soc=row['SocialBlock']
    #                 stats=blk_soc[0]
    #                 timeseries_dict=blk_soc[0]['timeseries']
    #                 # todo: si possono calcolare medie, dev stand, ecc
    #         else:
    #             track_social=None

    #         add_row['TrackedSocial']=track_social

            df_final=pd.concat([df_final, add_row])

    return df_final


def summary_stats(df=None, date_format='D', n_digits=2):

    '''
      date_format: show dates up to days https://numpy.org/doc/stable/reference/arrays.datetime.html#arrays-dtypes-dateunits
      n_digits: rounding digits for min, max, mean, ...
    '''

    import pandas as pd
    import numpy as np
    import warnings
    
    NUMERICS = ['number', 'int16', 'int32', 'int64', 'float16', 'float32', 'float64']
    DATES = ['datetimetz', 'datetime64', 'datetime']
    CATEGORICALS = ['object']
    BOOLS = ['bool']
    all_cols = df.columns

    tot_rows, tot_cols = df.shape
    pd.set_option('display.max_rows', tot_cols)

    num_cols = df.select_dtypes(include=NUMERICS).columns
    date_cols = df.select_dtypes(include=DATES).columns
    cat_cols = df.select_dtypes(include=CATEGORICALS).columns
    bool_cols = df.select_dtypes(include=BOOLS).columns

    # convert int64 to float (so as to evaluate mean, etc)
    for col in df.select_dtypes(include=['int64']).columns:
        df[col] = df[col].astype(float)    
    
    # Numerical stats
    num_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'INFs', 'ZEROs', 'MIN', 'MAX', 'MEAN',
                                      'STDDEV', 'MEDIAN', 'PERC1', 'PERC5', 'PERC95', 'PERC99', 'SUM'])
    for var in num_cols:

        val = df[var].values
        val = val[~np.isnan(val)]
        val = val[~np.isinf(val)]

        warnings.filterwarnings("ignore")
        perc = np.quantile(val, [0.01, 0.05, 0.95, 0.99])
        median = np.round(np.median(val), n_digits)
        warnings.filterwarnings("default")

        add_row = pd.DataFrame({'VARIABLE': var,
                                'TYPE': str(df[var].dtypes),
                                'OBS': tot_rows,
                                'UNIQUE': df[var].nunique(),
                                'NANs': df[var].isna().sum(),
                                'INFs': np.isinf(df[var].values).sum(),
                                'ZEROs': sum(df[var] == 0),
                                'MIN': np.round(val.min(), n_digits),
                                'MAX': np.round(val.max(), n_digits),
                                'MEAN': np.round(val.mean(), n_digits),
                                'STDDEV': np.round(val.std(), n_digits),
                                'MEDIAN': median,
                                'PERC1': np.round(perc[0], n_digits),
                                'PERC5': np.round(perc[1], n_digits),
                                'PERC95': np.round(perc[2], n_digits),
                                'PERC99': np.round(perc[3], n_digits),
                                'SUM': np.round(val.sum(), n_digits)
                               }, index = [0])

        num_stats = pd.concat([num_stats, add_row])


    # Categorical stats
    cat_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'BLANKs', 'VALUES_BY_FREQ'])

    for var in cat_cols:

        add_row = pd.DataFrame({'VARIABLE': var,
                                'TYPE': str(df[var].dtypes),
                                'OBS': tot_rows,
                                'UNIQUE': df[var].nunique(),
                                'NANs': df[var].isna().sum(),
                                'BLANKs': sum(df[var] == ''),
                                'VALUES_BY_FREQ': '|'.join(df[var].value_counts().index[:40].astype(str))
                               }, index = [0])

        cat_stats = pd.concat([cat_stats, add_row])


    # Boolean stats
    bool_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'NANs', 'MEAN', 'STDDEV', 'MEDIAN', 'PERC1',
                                       'PERC5', 'PERC95', 'PERC99', 'SUM', 'VALUES_BY_FREQ'])
    for var in bool_cols:

        val = df[var].dropna().values

        warnings.filterwarnings("ignore")
        perc = np.quantile(val.astype(int), [0.01, 0.05, 0.95, 0.99])
        median = np.round(np.median(val), n_digits)
        warnings.filterwarnings("default")        
                                                   
        add_row = pd.DataFrame({'VARIABLE': var,
                                'TYPE': str(df[var].dtypes),
                                'OBS': tot_rows,
                                'NANs': df[var].isna().sum(),
                                'MEAN': np.round(val.mean(), n_digits),
                                'STDDEV': np.round(val.std(), n_digits),
                                'MEDIAN': median,
                                'PERC1': perc[0],
                                'PERC5': perc[1],
                                'PERC95': perc[2],
                                'PERC99': perc[3],
                                'SUM': val.sum(),
                                'VALUES_BY_FREQ': ', '.join([str(k) + ': ' + str(v)
                                                             for k, v in df[var].dropna().value_counts().to_dict().items()])
                               }, index = [0])

        bool_stats = pd.concat([bool_stats, add_row])

    # Date stats
    date_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'MIN', 'MAX', 'MEDIAN', 'PERC1',
                                       'PERC5', 'PERC95', 'PERC99'])
    for var in date_cols:

        val = df[var].dropna().values
        val_str = np.datetime_as_string(val, unit=date_format)

        # calculation for median and quantile
        val_cnt = sorted(pd.Series(val_str).unique())
        mapping = dict(zip(val_cnt, range(len(val_cnt))))
        mapped = [mapping.get(v, v) for v in val_str]
        warnings.filterwarnings("ignore")
        med_ind = np.median(mapped).astype(int)
        warnings.filterwarnings("default")  

        if len(val) > 0:
        
            warnings.filterwarnings("ignore")
            perc = np.quantile(mapped, [0.01, 0.05, 0.95, 0.99]).astype(int)
            median = [k for k, v in mapping.items() if v == med_ind][0]
            warnings.filterwarnings("default")  
            add_row = pd.DataFrame({'VARIABLE': var,
                                    'TYPE': str(df[var].dtypes),
                                    'OBS': tot_rows,
                                    'UNIQUE': df[var].nunique(),
                                    'NANs': df[var].isna().sum(),
                                    'MIN': np.datetime_as_string(val.min(), unit=date_format),
                                    'MAX': np.datetime_as_string(val.max(), unit=date_format),
                                    'MEDIAN': median,
                                    'PERC1': [k for k, v in mapping.items() if v == perc[0]][0],
                                    'PERC5': [k for k, v in mapping.items() if v == perc[1]][0],
                                    'PERC95': [k for k, v in mapping.items() if v == perc[2]][0],
                                    'PERC99': [k for k, v in mapping.items() if v == perc[3]][0]
                                   }, index = [0])
        else:
            add_row = pd.DataFrame({'VARIABLE': var,
                            'TYPE': str(df[var].dtypes),
                            'OBS': tot_rows,
                            'UNIQUE': df[var].nunique(),
                            'NANs': df[var].isna().sum()
                       }, index = [0])

        date_stats = pd.concat([date_stats, add_row])

    # final stats
    all_col_set = ['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'INFs', 'ZEROs', 'BLANKs', 'MEAN', 'STDDEV', 'MIN', 
                                       'PERC1', 'PERC5', 'MEDIAN', 'PERC95', 'PERC99', 'MAX', 'SUM', 'VALUES_BY_FREQ']
    used_col_set = []
    final_stats = pd.DataFrame(columns=all_col_set)
    if num_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, num_stats])
        used_col_set.extend(num_stats.columns)
    if cat_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, cat_stats])
        used_col_set.extend(cat_stats.columns)
    if bool_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, bool_stats])
        used_col_set.extend(bool_stats.columns)
    if date_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, date_stats])
        used_col_set.extend(date_stats.columns)

    final_stats = final_stats[[x for x in all_col_set if x in np.unique(used_col_set)]]

    if final_stats['VARIABLE'].nunique() != final_stats.shape[0]:
        print('-- Duplicated variables found!')

    if final_stats['VARIABLE'].nunique() != tot_cols:
        print('-- Missing variables found:\n    ', '\n     '.join(set(all_cols) - set(final_stats['VARIABLE'].values)))

    final_stats['order'] = pd.Categorical(final_stats['VARIABLE'], categories = all_cols, ordered = True)
    final_stats = final_stats.sort_values(by='order').drop(columns=['order'])

    # add percentage to missing, inf, zeros, blank
    if 'NANs' in final_stats.columns:
        final_stats['NANs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['NANs'].values]
    if 'INFs' in final_stats.columns:
        final_stats['INFs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['INFs'].values]
    if 'ZEROs' in final_stats.columns:
        final_stats['ZEROs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['ZEROs'].values]
    if 'BLANKs' in final_stats.columns:
        final_stats['BLANKs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['BLANKs'].values]
    final_stats.fillna('', inplace=True)
    
    return final_stats

def convert_fund(x):
    
    if len(x) > 0:
        if '$' in x:
            out=int(float(x.replace('$', '').replace(',', '').strip()))
        else:
            out=x
    else:
        out=None
    
    return out

def extract_price(row):
    
    CURRENCY=['USD', 'EUR', 'ETH', 'BNB', 'USDT', 'BTC', 'CHF']
    
    error=''
    warning=''
    ticker=row['Ticker']
    price=(row['Price'].replace('$', 'USD').replace('€', 'EUR').replace('\u200b', '').replace('~', '')           
           .replace('.000.000.000', '000000000').replace('00.000.000', '00000000').replace('0.000.000', '0000000')
             .replace('.000.000', '000000').replace('00.000', '00000'))
    
    # split token and currency
    if ticker == '':
        ticker='xxxx1111yyyy'
    split_str=re.split('==|≈|=|—|:|＝', price)
    if len(split_str) == 1 and ticker in price:
        split_str=re.split('-', price)
    token_ind=[i for i, x in enumerate(split_str) if ticker in x and len(x)>0]
    currency_ind=[i for i, x in enumerate(split_str) if ticker not in x and len(x)>0]
    if len(token_ind) > 1 or len(currency_ind) > 1:
        retrieve_currency_ind=np.unique([i for i, y in enumerate(split_str) for x in CURRENCY if x in y]).tolist()
        if len(retrieve_currency_ind) == 1:
            currency_ind=retrieve_currency_ind
            token_ind=list(set(range(len(split_str))) - set(currency_ind))
            warning+='-multiple token or currency index recovered'
        else:
            error+='-multiple token or currency index '
    token_part= split_str[token_ind[0]] if token_ind else ''
    currency_part= split_str[currency_ind[0]] if currency_ind else ''

    # extract unit and currency
    token_unit=''
    if len(token_part) > 0:
        token_unit=token_part.replace(ticker, '').strip()
        if token_unit.startswith('0,'):
            token_unit=token_unit.replace('0,', '0.')
        else:
            token_unit=token_unit.replace(',', '')
        if re.sub('[A-Za-z]+', '', token_unit) != token_unit:   # try to remove mismatch in ticker
            token_unit=re.sub('[A-Za-z]+', '', token_unit)
            warning+='-adjusted ticker'
    if len(token_unit) == 0:
        token_unit='1'
    token_unit_num=None
    token_unit=token_unit.replace(' ', '').replace('[', '').replace(']', '')
    if '-' in token_unit:    # price range -> take average
        try:
            token_unit_num=sum([float(x) for x in token_unit.split('-')])/len(token_unit.split('-'))
            warning+='-token average'
        except:
            error+='-token average failed'
    else:
        try:
            token_unit_num=float(token_unit)
        except:
            error+='-token error float'
        if '-token error float' in error:   # try to remove . used as thousand separator
            try:
                token_unit_num=float(token_unit.replace('.000', '000'))
                warning+='-token dot thousand separator removed'
                error=error.replace('-token error float', '')
            except:
                pass
            
    currency_unit=None
    currency_lab=None
    currency_unit_num=None
    currency_part=currency_part.replace(' ', '')
    if len(currency_part) > 0:
        # check if decimal is . or ,
        if len(re.findall('[^0-9]0,', currency_part)) > 0 or currency_part.startswith('0,'):
            currency_part=currency_part.replace('0,', '0.')
        else:
            currency_part=currency_part.replace(',', '')
        currency_split=re.findall("(\d*\.?\d+|[A-Za-z]+)", currency_part)
        letters_count=[sum([x.isalpha() for x in y]) for y in currency_split]
        if len(currency_split) != 2:

            lab_ind=[i for i, x in enumerate(letters_count) if x > 0]
            check_lab=np.unique(np.array(currency_split)[lab_ind])
            if len(check_lab) > 1:
                error+='-currency range error multiple labels'
            elif len(check_lab) == 0:
                error+='-currency range error missing label'
            else:
                currency_lab=np.unique(np.array(currency_split)[lab_ind])[0]
                currency_unit=currency_part.replace(currency_lab, '').replace(' ', '')
                if '-' in currency_unit:    # price range -> take average
                    try:
                        currency_unit_num=sum([float(x) for x in currency_unit.split('-')])/len(currency_unit.split('-'))
                        warning+='-currency average'
                    except:
                        error+='-currency range error float'

        else:
            number_ind=letters_count.index(min(letters_count))
            lab_ind=letters_count.index(max(letters_count))
            currency_lab=currency_split[lab_ind]
            currency_unit=currency_split[number_ind].replace(' ', '')
            if currency_unit.startswith('0,'):
                currency_unit=currency_unit.replace('0,', '0.')
            else:
                currency_unit=currency_unit.replace(',', '')
            try:
                currency_unit_num=float(currency_unit)
            except:
                error+='-currency error float'
            if '-currency error float' in error:   # try to remove . used as thousand separator
                try:
                    currency_unit_num=float(currency_unit.replace('.000', '000'))
                    warning+='-currency dot thousand separator removed'
                    error=error.replace('-token error float', '')
                except:
                    pass
    
    if token_unit_num is None:
        error+='-missing token unit numeric'
    if currency_unit_num is None:
        error+='-missing currency unit numeric'
        
    
    return pd.Series({'token_unit': token_unit, 'token_unit_num': token_unit_num,
                      'currency_unit': currency_unit, 'currency_unit_num':currency_unit_num, 'currency_lab': currency_lab,
                      'token_part': token_part, 'currency_part': currency_part, 'error': error[1:], 'warning': warning[1:]})


def format_columns(format_df, cat_list=None, format_df_rows=0, results_folder=''):

    #### format FundRaised


    format_df.loc[format_df['url']=='https://icomarks.com/ico/cdrx', 'FundRaised']='19,000,000$'

    print('\n** Formatting "FundRaised"')
    missing_pre=sum(format_df['FundRaised']=='')
    format_df['FundRaised']=format_df['FundRaised'].map(convert_fund)
    format_df=format_df.rename(columns={'FundRaised': 'FundRaisedUSD'})
    missing_post=format_df['FundRaisedUSD'].isna().sum()
    if missing_pre != missing_post:
        print(f'- FundRaisedUSD: expected missing: {missing_pre}  current missing: {missing_post}')
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### extract continent and subregion from country

    COUNTRY_MAPPING='.\\Data and papers\\country_to_continent.csv'  # https://worldpopulationreview.com/country-rankings/list-of-countries-by-continent
    MANUAL_MAPPING=pd.DataFrame({'original': ['ZBG', 'UK', 'USA', 'Global', 'Holland', 'Austria/Romania',
                                             'Delaware USA', 'CHE', 'Riga', 'UAE', 'Россия',
                                             'Wordwide', 'Kosovo', 'Düsseldorf', 'UAE, USA', 'World', 'BVI',
                                             'Amsterdam', 'London', 'Various', 'Latin America', 'England, UK',
                                             'Worldwide', 'DUBAI', 'Polska', 'Türkiye', 'Dubai',
                                             'UAE, Dubai', 'ShangHai', 'Toronto', 'Czechia', 'Scotland', 'Brasilia',
                                             'usa', 'Swaziland', 'ENGLAND', 'International', 'Italia', 'England',
                                             'Boulder, CO, San Francisco, CA, and Hyderabad, India', 'Slovak Republic',
                                             'Dubai, United Emirates', 'Ukraine, Finland, India', 'India, Nigeria and US',
                                             'USA, Canada, Philippines', 'US'],
                               'new': ['Hong Kong', 'United Kingdom', 'United States', 'Worldwide', 'Netherlands', 'Austria',
                                       'United States', 'Switzerland', 'Latvia', 'United Arab Emirates', 'Russia',
                                      'Worldwide', 'Albania', 'Germany', 'United Arab Emirates', 'Worldwide', 'British Virgin Islands',
                                      'Netherlands', 'United Kingdom', 'Worldwide', 'Brazil', 'United Kingdom',
                                      'Worldwide', 'United Arab Emirates', 'Poland', 'Turkey', 'United Arab Emirates',
                                      'United Arab Emirates', 'China', 'Canada', 'Czech Republic', 'United Kingdom', 'Brazil',
                                      'United States', 'Eswatini', 'United Kingdom', 'Worldwide', 'Italy', 'United Kingdom',
                                      'United States', 'Slovakia',
                                      'United Arab Emirates', 'Ukraine', 'India',
                                      'United States', 'United States']})

    mapping=pd.read_csv(COUNTRY_MAPPING)[['country', 'region', 'subregion']].drop_duplicates()
    mapping=pd.concat([mapping, pd.DataFrame({'country': 'Worldwide', 'region': 'All', 'subregion': 'All'}, index=[mapping.shape[0]])])
    choices=mapping['country'].values.tolist()

    print('\n** Formatting "Country"')
    df_mapping=pd.DataFrame({'Country': format_df[format_df['Country'] != '']['Country'].unique()})
    df_mapping['Country_adj']=df_mapping['Country']
    for _, row in MANUAL_MAPPING.iterrows():
        df_mapping['Country_adj'].replace(row['original'], row['new'], inplace=True)
    df_mapping=df_mapping[df_mapping['Country_adj'] != '']
    df_mapping[['country', 'accuracy']]=df_mapping['Country_adj'].apply(lambda x: pd.Series(process.extractOne(x, choices)))
    print('- Mapped countries with low accuracy:')
    display(df_mapping.query('accuracy < 90').sort_values(by='accuracy'))

    format_df=format_df.merge((df_mapping.merge(mapping, on='country', how='left')[['Country', 'country', 'region', 'subregion']]
                               .rename(columns={'region': 'Region', 'subregion': 'SubRegion', 'country': 'country_fixed'})), on='Country', how='left')
    format_df[['Region', 'SubRegion']] = format_df[['Region', 'SubRegion']].fillna(value='')
    move_col = format_df.pop('Region')
    format_df.insert(format_df.columns.get_loc("Country")+1, 'Region', move_col)
    move_col = format_df.pop('SubRegion')
    format_df.insert(format_df.columns.get_loc("Region")+1, 'SubRegion', move_col)
    move_col = format_df.pop('country_fixed')
    format_df.insert(format_df.columns.get_loc("Country")+1, 'country_fixed', move_col)
    format_df.rename(columns={'Country': 'CountryOriginal', 'country_fixed': 'Country'}, inplace=True)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### dummy for social media

    print('\n** Formatting "SocialMedia"')
    unique, counts = np.unique([item for sublist in format_df['SocialMedia'].dropna() for item in sublist], return_counts=True)
    social_count=pd.DataFrame({'val': unique, 'count': counts}).sort_values(by='count', ascending=False)
    print('- Counts for SocialMedia dummy:')
    display(social_count)

    df=format_df[['url', 'SocialMedia']].copy().dropna()
    df_dummy=pd.DataFrame()
    for _, row in df.iterrows():
        dt=pd.get_dummies(row['SocialMedia'], drop_first=False, prefix='Social')
        dt.insert(0, 'url', row['url'])
        df_dummy=pd.concat([df_dummy, dt.groupby('url').sum()])
    df_dummy=df_dummy.fillna(0).add_suffix('Dummy')
    df_dummy.columns=df_dummy.columns.str.replace('_', '')
    if df_dummy.max().max() > 1:
        print('#### Warning: dummy for social media exceed 1')
    social_lab=df_dummy.columns
    df_dummy.reset_index(inplace=True)
    format_df=format_df.merge(df_dummy, on='url', how='left').drop(columns=['SocialMedia'])
    format_df[social_lab] = format_df[social_lab].fillna(value=0)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### convert Price to USD

    CURRENCY_ADJUST=pd.DataFrame({'original': ['ETHER', 'ETHERS', 'EHT', 'XML', 'USDUSD', 'EURO', 'EUREUR', 'WAVE',
                                               'USDBNB', 'USDBUSD', 'EH'],
                                'adjusted': ['ETH', 'ETH', 'ETH', 'XLM', 'USD', 'EUR', 'EUR', 'WAVES',
                                             'BNB', 'BUSD', 'ETH']})

    format_df.loc[format_df['Ticker']=='YFNFT', 'ICOPrice']='0.05 USD'
    format_df.loc[format_df['Ticker']=='YFNFT', 'TokenTotSupply']='73339 YFNFT'
    format_df.loc[format_df['Ticker']=='PRO6, PSIX', 'IEOPrice']='0.075 USD'
    format_df.loc[format_df['Ticker']=='ROX/ROX', 'ICOPrice']='0.05 USD'
    format_df.loc[format_df['Ticker']=='BMBCoin', 'ICOPrice']='0.1 USD'
    format_df.loc[format_df['Ticker']=='W2TW', 'ICOPrice']='0.65 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/blockchainaero', 'ICOPrice']='0.0002 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/abr', 'ICOPrice']='0.5 USD'
    format_df.loc[format_df['Ticker']=='WISE', 'ICOPrice']='15 WISE = 1 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/asseta', 'Ticker']='AST'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/future-coin-light', 'Ticker']='FTC'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/future-coin-light', 'Ticker']
    format_df.loc[format_df['url']=='https://icomarks.com/ico/cointour', 'ICOPrice']='1 COT = 0.0001 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/portalverse', 'ICOPrice']='1 PORV = 10 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/floppa-token', 'ICOPrice']='$6.3 = 1000 FLOP'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/hybrid-betting', 'ICOPrice']='1 HYB = 0.17$'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/bitcoincopy', 'ICOPrice']='0.31$ = BTCC'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/izombie-universe', 'ICOPrice']='1 BNB = 5800 iZOMBIE'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/syndiqate', 'ICOPrice']='1 SQAT = $0.20'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/metacade', 'ICOPrice']='0.012 USDT = 1 MCADE'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/verifi-defi', 'ICOPrice']='$0.004'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/open-box-swap', 'ICOPrice']='1 OBOX= 0.0225 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/suapp', 'ICOPrice']='1 SUP =  0.025 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/onlycumies', 'ICOPrice']='1 BNB = 1,000,000 ONLYCUMIES'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/calvaria', 'ICOPrice']='1 USDT = 100.0 RIA'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/one-game', 'ICOPrice']='$0.0075 = OGT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/mcash-exchange-mbcash-ico', 'ICOPrice']='1 MBCash = $0.05'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/fightout', 'ICOPrice']='40 FGHT = 1 USDT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/yesil-token', 'ICOPrice']='$0.0020'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/digital-dollar', 'ICOPrice']='$0.10'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/juniverse-token', 'ICOPrice']='$1'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/calmoairphoenix', 'ICOPrice']='0.01 $'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/promodio', 'ICOPrice']='$0.005'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ocreata', 'ICOPrice']='1 BNB = 1,000,000 OCREATA'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/digital-euro', 'ICOPrice']='0.10 EUR'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/kaon', 'ICOPrice']='$0.0005'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/terrarium', 'ICOPrice']='1 TRM = 0.0005 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/blockapp', 'ICOPrice']='1 USDT = 1000 BAPP'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/bellatrix-network', 'ICOPrice']='330,000 BTX = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/gisc-loancoin-network', 'ICOPrice']='20,000 GISC LoanCoin/GIS = 1 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/astra', 'ICOPrice']='1 ETH = 1,000 STAR'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/nodis', 'ICOPrice']='NODIS = 0.1105 GA'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/sensing-exchange-capital', 'ICOPrice']='1 SEC == 0.00166 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/playrs', 'ICOPrice']='1 ETH = 4,000 PLAY'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/lucisdollar', 'ICOPrice']='1 BTC = 400000000 LUCD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/beuthereum', 'Ticker']='BCH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/lottechain', 'Ticker']='LEN'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/revenue-coin', 'ICOPrice']='1 RVC = 0.012 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/metadollar', 'ICOPrice']='1 USDME = $0.02'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/interledgerswap', 'ICOPrice']='1 BTC = 43049.896370 XRP'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/safemoonred', 'ICOPrice']='25000 SMR = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/bonzai-technology', 'ICOPrice']='3,750,000,000 BONZAI = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/zeller', 'ICOPrice']='1 ZLR = 0.0000165012 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/monkeycola', 'ICOPrice']='1 MKC = 0.3 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/metarobotwarrior', 'ICOPrice']='1 MRTW = 0.0001 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/wordpool', 'ICOPrice']='500000 WORD = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/metahex', 'ICOPrice']='1,000 MTX = 8 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/hive-power', 'ICOPrice']='1 ETH = 2432 HVT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/shibacute', 'ICOPrice']='1 BNB = 500000000000 SCUTE'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coin-for-nature', 'ICOPrice']='1 BNB = 3200000 COFN'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/flexq', 'ICOPrice']='1 BNB = 5210 FLQ'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/velic', 'ICOPrice']='0.01 VELT = 1 USDT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/civil', 'ICOPrice']='1 CIV = 0.0001 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/alsonft', 'ICOPrice']='1 AlsoNFT = 0.01 USDC'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/wiki-simplify', 'ICOPrice']='1 WSY = 0.15 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/2021coin', 'ICOPrice']='0.64 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/clear-crystal-token', 'ICOPrice']='0.0001 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/romad-endpoint-defense', 'ICOPrice']='1 RBDT = 0.00288000 USDT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/crypto-market-ads', 'ICOPrice']='1 CMA coin = 0.01 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/custodi', 'ICOPrice']='0.60 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/micropad', 'ICOPrice']='0.075 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/neutro', 'IEOPrice']='1 NTO = 0.6 USDT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/genevieve-vc1518213868', 'ICOPrice']='1 GXVC = 0.10 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ixoracoin', 'ICOPrice']='1 BNB = 12,500,000 IXO'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/Apillon', 'IEOPrice']='0,40 $'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/earth-token', 'ICOPrice']='4000 ETN = 0.1 BTC'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/wall-street-coin', 'ICOPrice']='1 WSC = 0.75 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/globetrotter', 'ICOPrice']='1 GTT = 0.20 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/bluetherium', 'ICOPrice']=''
    format_df.loc[format_df['url']=='https://icomarks.com/ico/safecrypt', 'ICOPrice']='1 SFC =  0.00006 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/plaak', 'ICOPrice']='1 PLK = 1.4174 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/quanta-networks', 'ICOPrice']='1 QN = 0.70 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/true-gold-coin', 'ICOPrice']='1 TGC = 17.36 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/gauss-gang', 'ICOPrice']='1 GANG = 0.07 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/rpstoken', 'ICOPrice']='1 RPS= 0.000002 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/dragonnest', 'ICOPrice']='1 DRAGONNEST = 0.1 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/holiday', 'ICOPrice']='1 NEO = 1000 Holiday Coin'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ip-gold', 'ICOPrice']='1 IPG = 1 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/flexion', 'ICOPrice']='1 FXN = 0.025 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/zantepay', 'ICOPrice']='1 ZNX = 0.310000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/christ-coins', 'ICOPrice']='1 CCLC = 0.09 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coingrid', 'ICOPrice']='1 CGT = 0.044396 USD'


    print('\n** Formatting "ICOPrice", "IEOPrice", "STOPrice"')
    check_single_value_in_row=format_df[['ICOPrice','IEOPrice', 'STOPrice']].apply(lambda x: sum(x!=''), axis=1).max()
    if check_single_value_in_row > 1:
        print('- Rows with more than one "ICOPrice", "IEOPrice", "STOPrice"')
    date_df=format_df[[col for col in format_df.columns if 'date' in col.lower()]].replace('TBA', '').replace(np.nan, '').fillna(pd.to_datetime('1 Jan 2150'))
    date_df['RefDate']=date_df.apply(lambda x: min([y for y in x if type(y) != str]), axis=1)
    # date_df.replace(pd.to_datetime('1 Jan 2150'), np.nan, inplace=True)
    price_df=format_df[['url', 'Ticker', 'ICOPrice','IEOPrice', 'STOPrice']].copy()
    price_df=pd.concat([price_df, date_df['RefDate']], axis=1)
    price_df['RefDate']=price_df['RefDate'].dt.year.astype(str)+'_'+price_df['RefDate'].dt.month.astype(str).str.zfill(2)
    price_df['RefDate'].replace('2150_01', 'last_avail', inplace=True)
    price_df['Price']=price_df['ICOPrice']+price_df['IEOPrice']+price_df['STOPrice']
    price_df=price_df[price_df['Price']!='']
    price_df['check_split']=price_df.apply(lambda x: len(re.split('==|≈|=|—', x['Price'])), axis=1)
    check_len=price_df.query('check_split > 2')
    if check_len.shape[0] > 0:
        print('- Found rows with wrong entries ("PriceUSD" will not be evaluated):')
        display(check_len)
        price_df=price_df[~price_df['url'].isin(check_len['url'].values)]
    price_df=pd.concat([price_df, price_df.apply(extract_price, axis=1)], axis=1)
    print('- Errors when parsing "ICOPrice", "IEOPrice", "STOPrice":')
    display(price_df['error'].value_counts())
    price_df.to_csv(os.path.join(results_folder, '01c_ICOmarks_formatted_price_error_log.csv'), index=False, sep=';')
    print('- Error log saved in', os.path.join(results_folder, '01c_ICOmarks_formatted_price_error_log.csv'))
    price_df=price_df[price_df['error'] == '']
    price_df['currency_lab']=price_df['currency_lab'].str.upper()
    for i, row in CURRENCY_ADJUST.iterrows():
        price_df['currency_lab'].replace(row['original'], row['adjusted'], inplace=True)
    price_df=price_df.merge(cat_list[['url', 'StartDate']], on='url', how='left')  # check if "last_avail" is assigned only on TBA
    if not (price_df[price_df['RefDate']=='']['StartDate'].unique()=='TBA').all():
        print('- Warning: check missing "RefDate", "TBA" as date is automatically assigned and last available FX rate is taken')
    # ETH-USD https://finance.yahoo.com/quote/ETH-USD/history
    # BNB-USD https://finance.yahoo.com/quote/BNB-USD/history
    # USDT-USD https://finance.yahoo.com/quote/USDT-USD/history
    # BTC-USD https://finance.yahoo.com/quote/BTC-USD/history
    # XLM-USD https://finance.yahoo.com/quote/XLM-USD/history
    # TRX-USD https://finance.yahoo.com/quote/TRX-USD/history
    # BUSD-USD https://finance.yahoo.com/quote/BUSD-USD/history
    # KRW-USD https://finance.yahoo.com/quote/KRW-USD/history
    # NEO-USD https://finance.yahoo.com/quote/NEO-USD/history
    # MATIC-USD https://finance.yahoo.com/quote/MATIC-USD/history
    # USDC-USD https://finance.yahoo.com/quote/USDC-USD/history
    # WAVES-USD https://finance.yahoo.com/quote/WAVES-USD/history
    # BTS-USD https://finance.yahoo.com/quote/BTS-USD/history
    # VET-USD https://finance.yahoo.com/quote/VET-USD/history
    # ICX-USD https://finance.yahoo.com/quote/ICX-USD/history
    # LNC-USD https://finance.yahoo.com/quote/LNC-USD/history
    # WETH-USD https://finance.yahoo.com/quote/WETH-USD/history
    # DOT-USD https://finance.yahoo.com/quote/DOT-USD/history
    # AVAX-USD https://finance.yahoo.com/quote/AVAX-USD/history
    # AV-USD https://finance.yahoo.com/quote/AV-USD/history
    # EUR-USD https://www.investing.com/currencies/eur-usd-historical-data
    # GBP-USD https://www.investing.com/currencies/gbp-usd-historical-data
    # CHF-USD https://www.investing.com/currencies/chf-usd-historical-data
    # AUD-USD https://www.investing.com/currencies/aud-usd-historical-data
    # SGD-USD https://www.investing.com/currencies/sgd-usd-historical-data
    fx_df=pd.DataFrame()
    # from yahoo finance
    for file in ['ETH-USD', 'BNB-USD', 'USDT-USD', 'BTC-USD', 'XLM-USD', 'TRX-USD', 'BUSD-USD', 'KRW-USD', 'NEO-USD',
                 'MATIC-USD', 'USDC-USD', 'WAVES-USD', 'BTS-USD', 'VET-USD', 'ICX-USD', 'LNC-USD', 'WETH-USD',
                 'DOT-USD', 'AVAX-USD', 'AV-USD']:
        dd=pd.read_csv(os.path.join('.\\Data and papers', file+'.csv'), parse_dates=['Date'])
        dd.insert(0, 'currency_lab', file.replace('-USD', ''))
        dd['RefDate']=dd['Date'].dt.year.astype(str)+'_'+dd['Date'].dt.month.astype(str).str.zfill(2)
        dd['USDFx']=(dd['Open']+dd['Adj Close']) / 2
        fx_df=pd.concat([fx_df, dd[['currency_lab', 'RefDate', 'USDFx']]])
    # from investing.com
    for file in ['CHF_USD Historical Data', 'EUR_USD Historical Data', 'GBP_USD Historical Data',
                 'AUD_USD Historical Data', 'SGD_USD Historical Data']:
        dd=pd.read_csv(os.path.join('.\\Data and papers', file+'.csv'), parse_dates=['Date'])
        dd.insert(0, 'currency_lab', file.replace('_USD Historical Data', ''))
        dd['RefDate']=dd['Date'].dt.year.astype(str)+'_'+dd['Date'].dt.month.astype(str).str.zfill(2)
        dd['USDFx']=dd['Price']
        fx_df=pd.concat([fx_df, dd[['currency_lab', 'RefDate', 'USDFx']]])    
    fx_df=fx_df.groupby(['currency_lab', 'RefDate'], as_index=False).first()     # last month can have multiple values
    fx_df=pd.concat([fx_df,
                     (fx_df.sort_values(by='RefDate', ascending=False).groupby('currency_lab', as_index=False)
                      .agg(USDFx=('USDFx', lambda x: x.head(36).mean())).assign(RefDate='last_avail'))])
    # drop currency not included in fx
    avail_curr=fx_df['currency_lab'].unique().tolist()+['USD']
    remov=price_df[~price_df['currency_lab'].isin(avail_curr)].shape[0]
    print(f'- {remov} rows removed because currency FX rate not available')
    display(price_df[~price_df.currency_lab.isin(avail_curr)].currency_lab.value_counts())
    price_df=price_df[price_df['currency_lab'].isin(avail_curr)]
    # merge fx_df
    price_df=price_df.merge(fx_df, on=['currency_lab', 'RefDate'], how='left')
    price_df.loc[price_df['currency_lab'] == 'USD', 'USDFx']=1
    approx_fx=price_df[price_df['USDFx'].isna()]
    if approx_fx.shape[0] > 0:
        print('- Taking closest available FX rate for', approx_fx.shape[0], 'rows. Currency:', approx_fx['currency_lab'].unique())
        for i, row in approx_fx.iterrows():
            date=pd.to_datetime(row['RefDate'], format='%Y_%m')
            curr=row['currency_lab']
            match_df=fx_df[(fx_df['currency_lab']==curr) & (fx_df['RefDate'] != 'last_avail')].copy()
            match_df['Date']=pd.to_datetime(match_df['RefDate'], format='%Y_%m')
            fx=match_df[match_df['Date'] == min(match_df['Date'], key=lambda x: (x>date, abs(x-date)))]['USDFx'].values[0]
            price_df.loc[price_df['url']==row['url'], 'USDFx']=fx
    if price_df['USDFx'].isna().sum() > 0:
        print('-', price_df['USDFx'].isna().sum(), 'missing USD Fx rate found. Rows will be removed')
        price_df=price_df[~price_df['USDFx'].isna()]
    print(f'- Price available for {price_df.shape[0]} entries')
    price_df.to_csv(os.path.join(results_folder, '01c_ICOmarks_formatted_price_log.csv'), index=False, sep=';')
    print('- Price log saved in', os.path.join(results_folder, '01c_ICOmarks_formatted_price_log.csv'))
    # convert to USD
    price_df['PriceUSD']=price_df['currency_unit_num'] / price_df['token_unit_num'] * price_df['USDFx']
    if price_df['PriceUSD'].isna().sum() > 0 or price_df['PriceUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "PriceUSD"')
    # merge with format df
    format_df=format_df.merge(price_df[['url', 'PriceUSD']], on='url', how='left')
    move_col = format_df.pop('PriceUSD')
    format_df.insert(format_df.columns.get_loc("STOPrice")+1, 'PriceUSD', move_col)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### convert PreSalePrice to USD

    format_df.loc[format_df['url']=='https://icomarks.com/ico/bitether', 'PreSalePrice']='1 ETH = 615 BTT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/setcoin', 'PreSalePrice']='1 USD = 10 SET'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/sensitrust', 'PreSalePrice']='0.05 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/inmusik', 'PreSalePrice']='0.10 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/swapy-network', 'PreSalePrice']='0.57 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/phuket-holiday-coin', 'PreSalePrice']='1 PHC = 0.125 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/holiday', 'PreSalePrice']='1 NEO = 1500 Holiday Coin'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/immvrse', 'PreSalePrice']='0.20 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/linkercoin', 'PreSalePrice']='1 LNC  = 0.0003 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/metachessgame', 'PreSalePrice']='1 MTCG = $0.0035'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/floppa-token', 'PreSalePrice']='$5.4 = 1000 FLOP'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/bonzai-technology', 'PreSalePrice']='4166666666 BONZAI = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/lawrencium-token', 'PreSalePrice']='4000000 XLW = 1BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/renewable-energy-for-all', 'PreSalePrice']='14,000 REFA = 1 BSC'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/zeller', 'PreSalePrice']='1 ZLR = 0.0000124198 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/metarobotwarrior', 'PreSalePrice']='1MRTW = 0.00005 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/neutro', 'PreSalePrice']='1 NTO = 0.6 USDT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/dlife', 'PreSalePrice']='1 DLIFE=$0.028'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ccecoin1541800979', 'PreSalePrice']='1 CCE = $0.10'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/miningwatchdog-smartchain-token', 'PreSalePrice']='$0.5 = 1 MSC'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/the-sports-bet', 'PreSalePrice']='1 SBET = $0.0035'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/axl-inu', 'PreSalePrice']='$0.00075 = 1AXL'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/porn', 'PreSalePrice']='1 PORN = $ 0.006'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/mtw-games-token', 'PreSalePrice']='0.05 USDT = 1MTW'
    # format_df.loc[format_df['url']=='', 'PreSalePrice']=''

    print('\n** Formatting "PreSalePrice"')
    price_df=format_df[['url', 'Ticker', 'PreSalePrice']].copy()
    price_df=pd.concat([price_df, date_df['RefDate']], axis=1)
    price_df['RefDate']=price_df['RefDate'].dt.year.astype(str)+'_'+price_df['RefDate'].dt.month.astype(str).str.zfill(2)
    price_df['RefDate'].replace('2150_01', 'last_avail', inplace=True)
    price_df['Price']=price_df['PreSalePrice']
    price_df=price_df[price_df['Price']!='']
    price_df['check_split']=price_df.apply(lambda x: len(re.split('==|≈|=|—', x['Price'])), axis=1)
    check_len=price_df.query('check_split > 2')
    if check_len.shape[0] > 0:
        print('- Found rows with wrong entries ("PreSalePriceUSD" will not be evaluated):')
        display(check_len)
        price_df=price_df[~price_df['url'].isin(check_len['url'].values)]
    price_df=pd.concat([price_df, price_df.apply(extract_price, axis=1)], axis=1)
    print('- Errors when parsing "PreSalePrice":')
    display(price_df['error'].value_counts())

    price_df.to_csv(os.path.join(results_folder, '01c_ICOmarks_formatted_PreSaleprice_error_log.csv'), index=False, sep=';')
    print('- Error log saved in', os.path.join(results_folder, '01c_ICOmarks_formatted_PreSaleprice_error_log.csv'))
    price_df=price_df[price_df['error'] == '']
    price_df['currency_lab']=price_df['currency_lab'].str.upper()
    for i, row in CURRENCY_ADJUST.iterrows():
        price_df['currency_lab'].replace(row['original'], row['adjusted'], inplace=True)
    price_df=price_df.merge(cat_list[['url', 'StartDate']], on='url', how='left')  # check if "last_avail" is assigned only on TBA
    if not (price_df[price_df['RefDate']=='']['StartDate'].unique()=='TBA').all():
        print('- Warning: check missing "RefDate", "TBA" as date is automatically assigned and last available FX rate is taken')
    # drop currency not included in fx
    avail_curr=fx_df['currency_lab'].unique().tolist()+['USD']
    remov=price_df[~price_df['currency_lab'].isin(avail_curr)].shape[0]
    print(f'- {remov} rows removed because currency FX rate not available')
    price_df=price_df[price_df['currency_lab'].isin(avail_curr)]
    # merge fx_df
    price_df=price_df.merge(fx_df, on=['currency_lab', 'RefDate'], how='left')
    price_df.loc[price_df['currency_lab'] == 'USD', 'USDFx']=1
    approx_fx=price_df[price_df['USDFx'].isna()]
    if approx_fx.shape[0] > 0:
        print('- Taking closest available FX rate for', approx_fx.shape[0], 'rows. Currency:', approx_fx['currency_lab'].unique())
        for i, row in approx_fx.iterrows():
            date=pd.to_datetime(row['RefDate'], format='%Y_%m')
            curr=row['currency_lab']
            match_df=fx_df[(fx_df['currency_lab']==curr) & (fx_df['RefDate'] != 'last_avail')].copy()
            match_df['Date']=pd.to_datetime(match_df['RefDate'], format='%Y_%m')
            fx=match_df[match_df['Date'] == min(match_df['Date'], key=lambda x: (x>date, abs(x-date)))]['USDFx'].values[0]
            price_df.loc[price_df['url']==row['url'], 'USDFx']=fx
    if price_df['USDFx'].isna().sum() > 0:
        print('-', price_df['USDFx'].isna().sum(), 'missing USD Fx rate found. Rows will be removed')
        price_df=price_df[~price_df['USDFx'].isna()]
    print(f'- Price available for {price_df.shape[0]} entries')
    price_df.to_csv(os.path.join(results_folder, '01c_ICOmarks_formatted_PreSaleprice_log.csv'), index=False, sep=';')
    print('- Price log saved in', os.path.join(results_folder, '01c_ICOmarks_formatted_PreSaleprice_log.csv'))
    # convert to USD
    price_df['PreSalePriceUSD']=price_df['currency_unit_num'] / price_df['token_unit_num'] * price_df['USDFx']
    if price_df['PreSalePriceUSD'].isna().sum() > 0 or price_df['PreSalePriceUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "PreSalePriceUSD"')
    # merge with format df
    format_df=format_df.merge(price_df[['url', 'PreSalePriceUSD']], on='url', how='left')
    move_col = format_df.pop('PreSalePriceUSD')
    format_df.insert(format_df.columns.get_loc("PreSalePrice")+1, 'PreSalePriceUSD', move_col)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### convert Hard/Soft Cap

    format_df.loc[format_df['url']=='https://icomarks.com/ico/finebit-token', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/finebit-token', 'FundHardCap']='7000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/eshop', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/eshop', 'FundHardCap']='12000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/cardonio', 'FundHardCap']='360000000 CFGT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/cardonio', 'FundSoftCap']='220000000 CFGT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/masternode-invest', 'FundHardCap']='6500000 MS'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/united-farmers-x', 'FundHardCap']='205000000 UFX'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/123swap', 'FundHardCap']='40000000 123'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/konios-project', 'FundHardCap']='29000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/eddmate-token', 'FundHardCap']=''
    format_df.loc[format_df['url']=='https://icomarks.com/ico/smrt', 'FundHardCap']='53000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/yaffa', 'FundHardCap']='160000000 ENFT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ethichub', 'FundHardCap']='2,000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/aurora', 'FundHardCap']='5675200 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/levelnet', 'FundHardCap']='12000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/weekend-millionaires-club', 'FundHardCap']='2000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ebankx', 'FundHardCap']='78500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/baby-musk-coin', 'FundHardCap']='1544000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/x3-protocol', 'FundHardCap']='28890000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/mira', 'FundHardCap']='20000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/gameflip', 'FundHardCap']='12000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/beuthereum', 'FundHardCap']='8000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/altair-vr', 'FundHardCap']='10,000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/kiwilemon', 'FundHardCap']='USD 20000000'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/quanta-networks', 'FundHardCap']='300000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/iziing', 'FundHardCap']='30000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/stellerro', 'FundHardCap']='5000000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/tellefinance', 'FundHardCap']='3500000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/digital-euro', 'FundHardCap']='100000000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coinplace', 'FundHardCap']='20000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/fit-token', 'FundHardCap']='67000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/hut34-project', 'FundHardCap']='60000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/orfeus-network', 'FundHardCap']='50000 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/giga-giving', 'FundHardCap']='12000000 GC'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coinseed', 'FundHardCap']='1500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/hycon', 'FundSoftCap']='13900000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/gelios', 'FundSoftCap']='1000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ethichub', 'FundSoftCap']='1000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/levelnet', 'FundSoftCap']='1500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/ebankx', 'FundSoftCap']='1000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/baby-musk-coin', 'FundSoftCap']='1544000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/x3-protocol', 'FundSoftCap']='$28890000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/mira', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/altair-vr', 'FundSoftCap']='500 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/healthureum', 'FundSoftCap']='15000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/kiwilemon', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/quanta-networks', 'FundSoftCap']='20000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/iziing', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/stellerro', 'FundSoftCap']='500000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/digital-euro', 'FundSoftCap']='10000000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/orfeus-network', 'FundSoftCap']='1000 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/libra-ecosystem', 'FundSoftCap']=''
    format_df.loc[format_df['url']=='https://icomarks.com/ico/libra-ecosystem', 'FundHardCap']=''
    format_df.loc[format_df['url']=='https://icomarks.com/ico/armacoin', 'FundHardCap']='2380000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/global-innovative-solutions', 'FundHardCap']='2100000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/aliencloud', 'FundHardCap']='600000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/citicash', 'FundHardCap']='19500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/copernic', 'FundHardCap']='3200000 PLN'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/edenbest', 'FundHardCap']='16875 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/edenbest', 'FundSoftCap']='11812 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/arealeum', 'FundHardCap']='73392857 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/bolton-coin-bfcl', 'FundHardCap']='2198960 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/chainpals', 'FundHardCap']='2425000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/sleekplay', 'FundHardCap']='26000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/x3-protocol', 'FundSoftCap']='28890000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/citicash', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/escotoken', 'FundSoftCap']='16000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/posschain', 'FundSoftCap']='1725000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/brickken', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/Metacoms', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/flexion', 'FundHardCap']='5400000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/flexion', 'FundSoftCap']='900000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/flexion', 'FundHardCap']='2000 BNB'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/fanchain', 'FundHardCap']='330000000 FANZ'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/b-money', 'FundHardCap']='788190 BMNY'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/micropad', 'FundHardCap']='850000 MICROPAD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/custodi', 'FundHardCap']='35000000 Custodi Cash Token'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/fanchain', 'FundSoftCap']='50000000 FANZ'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/b-money', 'FundSoftCap']='118220 BMNY'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/micropad', 'FundSoftCap']='425000 MICROPAD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/cakepad', 'FundSoftCap']='80000000 Cakepad'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coingrid', 'FundHardCap']='10000000 CGT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coingrid', 'FundSoftCap']='70000000 CGT'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/cdrx', 'FundHardCap']='100000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/cdrx', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coinchase', 'FundHardCap']='41900 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/coinchase', 'FundSoftCap']='400 ETH'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/dago-mining', 'FundHardCap']='20000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.com/ico/dago-mining', 'FundSoftCap']='4000000 USD'

    print('\n** Formatting "FundHardCap" and "FundSoftCap"')
    date_df=format_df[[col for col in format_df.columns if 'date' in col.lower()]].replace('TBA', '').replace(np.nan, '').fillna(pd.to_datetime('1 Jan 2150'))
    date_df['RefDate']=date_df.apply(lambda x: min([y for y in x if type(y) != str]), axis=1)
    # date_df.replace(pd.to_datetime('1 Jan 2150'), np.nan, inplace=True)
    price_df=format_df[['url', 'Ticker', 'FundHardCap', 'FundSoftCap']].copy()
    price_df=pd.concat([price_df, date_df['RefDate']], axis=1)
    price_df['RefDate']=price_df['RefDate'].dt.year.astype(str)+'_'+price_df['RefDate'].dt.month.astype(str).str.zfill(2)
    price_df['RefDate'].replace('2150_01', 'last_avail', inplace=True)
    price_df=pd.concat([price_df[['url', 'Ticker', 'RefDate', 'FundHardCap']].assign(type='FundHardCap').rename(columns={'FundHardCap': 'Price'}),
              price_df[['url', 'Ticker', 'RefDate', 'FundSoftCap']].assign(type='FundSoftCap').rename(columns={'FundSoftCap': 'Price'})])
    price_df=price_df[price_df['Price']!='']
    price_df['check_split']=price_df.apply(lambda x: len(re.split('==|≈|=|—', x['Price'])), axis=1)
    check_len=price_df.query('check_split > 2')
    if check_len.shape[0] > 0:
        print('- Found rows with wrong entries ("FundHardCap" or "FundSoftCap" will not be evaluated):')
        display(check_len)
        price_df=price_df[~price_df['url'].isin(check_len['url'].values)]
    price_df=pd.concat([price_df, price_df.apply(extract_price, axis=1)], axis=1)
    # try to recover missing currency from ICOPrice, IEOPrice, STOPrice
    priceusd_df=pd.read_csv(os.path.join(results_folder, '01c_ICOmarks_formatted_price_log.csv'), sep=';')
    priceusd_df=priceusd_df[['url', 'currency_lab']].rename(columns={'currency_lab': 'currency_lab_PriceUSD'})
    priceusd_df=pd.concat([priceusd_df.assign(type='FundHardCap'), priceusd_df.assign(type='FundSoftCap')])
    row_exp=price_df.shape[0]
    price_df=price_df.merge(priceusd_df, on=['url', 'type'], how='left')
    price_df=price_df.merge(format_df[['url', 'PriceUSD', 'FundRaisedUSD']], on='url', how='left')
    if price_df.shape[0] != row_exp:
        print('###### warning: rows in "price_df" mismatch after join with "priceusd_df"')
    # try to recover Price without currency label by currency_lab from PriceUSD
    recov_df=price_df[(price_df['error']=='currency range error missing label-missing currency unit numeric') & (~price_df['currency_lab_PriceUSD'].isna())].copy()
    if len(recov_df) > 0:
        recov_df['Price']=recov_df['Price']+' '+recov_df['currency_lab_PriceUSD']
        recov_df=recov_df[['url', 'Ticker', 'RefDate', 'Price', 'type', 'currency_lab_PriceUSD', 'PriceUSD', 'FundRaisedUSD']]
        recov_df=pd.concat([recov_df, recov_df.apply(extract_price, axis=1)], axis=1)
        recov_df=recov_df[recov_df['error']=='']
        recov_df['warning']='currency recovered by PriceUSD'
        price_df=price_df[~price_df.index.isin(recov_df.index)]
        price_df=pd.concat([price_df, recov_df])
    if price_df.shape[0] != row_exp:
        print('###### warning: rows in "price_df" mismatch after join with "recov_df"')
    price_df.loc[(price_df['error'] == 'missing currency unit numeric') & (price_df['currency_part'] == ''), 'error']=''
    print('- Errors when parsing "FundHardCap" and "FundSoftCap":')
    display(price_df['error'].value_counts())

    price_df.to_csv(os.path.join(results_folder, '01c_ICOmarks_formatted_HardSoftCap_error_log.csv'), index=False, sep=';')
    print('- Error log saved in', os.path.join(results_folder, '01c_ICOmarks_formatted_HardSoftCap_error_log.csv'), end='\n')
    price_df=price_df[price_df['error'] == '']
    price_df['currency_lab']=price_df['currency_lab'].str.upper()
    for i, row in CURRENCY_ADJUST.iterrows():
        price_df['currency_lab'].replace(row['original'], row['adjusted'], inplace=True)
    price_df=price_df.merge(cat_list[['url', 'StartDate']], on='url', how='left')  # check if "last_avail" is assigned only on TBA
    if not (price_df[price_df['RefDate']=='']['StartDate'].unique()=='TBA').all():
        print('- Warning: check missing "RefDate", "TBA" as date is automatically assigned and last available FX rate is taken')
    ## evaluate Hard/SoftCap value in USD
    only_token_df=price_df[(price_df['token_part'] != '') & (price_df['currency_part'] == '')].copy()
    only_currency_df=price_df[(price_df['token_part'] == '') & (price_df['currency_part'] != '')].copy()
    both_df=price_df[(price_df['token_part'] != '') & (price_df['currency_part'] != '')].copy()
    ## only_token_df
    if len(only_token_df) + len(only_currency_df) +len(both_df) != len(price_df):
        print('- Warning: missing rows when splitting into "only_token_df", "only_currency_df" and "both_df"')
    only_token_df['CapUSD']=only_token_df['token_unit_num']*only_token_df['PriceUSD']
    if only_token_df.CapUSD.isna().sum() > 0:
        print(f'- {only_token_df.CapUSD.isna().sum()} rows in "only_token_df" (with Hard/Soft Cap in tokens) skipped because of missing "PriceUSD"')
    only_token_df=only_token_df[~only_token_df.CapUSD.isna()]
    print(f'- {only_token_df.shape[0]} rows remaing in "only_token_df" (with Hard/Soft Cap in tokens)')
    if only_token_df['CapUSD'].isna().sum() > 0 or only_token_df['CapUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "CapUSD" in "only_token_df"')
    ## only_currency_df
    # drop currency not included in fx
    avail_curr=fx_df['currency_lab'].unique().tolist()+['USD']
    remov=only_currency_df[~only_currency_df['currency_lab'].isin(avail_curr)].shape[0]
    if remov > 0:
        print(f'- {remov} rows in "only_currency_df" (with Hard/Soft Cap in currency) skipped because FX rate not available')
        display(price_df[~price_df.currency_lab.isin(avail_curr)].currency_lab.value_counts())
    only_currency_df=only_currency_df[only_currency_df['currency_lab'].isin(avail_curr)]
    # merge fx_df
    only_currency_df=only_currency_df.merge(fx_df, on=['currency_lab', 'RefDate'], how='left')
    only_currency_df.loc[only_currency_df['currency_lab'] == 'USD', 'USDFx']=1
    approx_fx=only_currency_df[only_currency_df['USDFx'].isna()]
    if approx_fx.shape[0] > 0:
        print('- Taking closest available FX rate for', approx_fx.shape[0], 'rows. Currency:', approx_fx['currency_lab'].unique())
        for i, row in approx_fx.iterrows():
            date=pd.to_datetime(row['RefDate'], format='%Y_%m')
            curr=row['currency_lab']
            match_df=fx_df[(fx_df['currency_lab']==curr) & (fx_df['RefDate'] != 'last_avail')].copy()
            match_df['Date']=pd.to_datetime(match_df['RefDate'], format='%Y_%m')
            fx=match_df[match_df['Date'] == min(match_df['Date'], key=lambda x: (x>date, abs(x-date)))]['USDFx'].values[0]
            only_currency_df.loc[only_currency_df['url']==row['url'], 'USDFx']=fx
    if only_currency_df['USDFx'].isna().sum() > 0:
        print('-', only_currency_df['USDFx'].isna().sum(), 'missing USD Fx rate found. Rows will be removed')
        only_currency_df=only_currency_df[~only_currency_df['USDFx'].isna()]
    print(f'- {only_currency_df.shape[0]} rows remaing in "only_currency_df" (with Hard/Soft Cap in currency)')
    # convert to USD
    only_currency_df['CapUSD']=only_currency_df['currency_unit_num'] * only_currency_df['USDFx']
    if only_currency_df['CapUSD'].isna().sum() > 0 or only_currency_df['CapUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "CapUSD" in "only_currency_df"')
    ## both_df
    if len(both_df) > 0:
        print(f'- All {len(both_df)} rows in "both_df" (with Hard/Soft Cap in currency AND token) skipped because of mismatch')

    price_df=pd.concat([only_currency_df, only_token_df])
    print(f'\n- Hard/Soft Cap available for {price_df.shape[0]} entries')
    price_df.to_csv(os.path.join(results_folder, '01c_ICOmarks_formatted_HardSoftCap_log.csv'), index=False, sep=';')
    print('- Price log saved in', os.path.join(results_folder, '01c_ICOmarks_formatted_HardSoftCap_log.csv'))
    # merge with format df
    rows=price_df.shape[0]
    price_df=price_df[['url', 'type', 'CapUSD']].pivot(index='url',columns='type',values='CapUSD').reset_index().rename(columns={'FundHardCap': 'FundHardCapUSD', 'FundSoftCap': 'FundSoftCapUSD'})
    if len(price_df) * 2 - price_df.drop(columns='url').isna().sum().sum() != rows:
        print('###### warning: spreading price_df generated unexpected missing')
    format_df=format_df.merge(price_df[['url', 'FundHardCapUSD', 'FundSoftCapUSD']], on='url', how='left')
    move_col = format_df.pop('FundHardCapUSD')
    format_df.insert(format_df.columns.get_loc("FundHardCap")+1, 'FundHardCapUSD', move_col)
    move_col = format_df.pop('FundSoftCapUSD')
    format_df.insert(format_df.columns.get_loc("FundSoftCap")+1, 'FundSoftCapUSD', move_col)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')

    return format_df


def set_global_logging_level(level=logging.ERROR, prefices=[""]):
    """
    Override logging levels of different modules based on their name as a prefix.
    It needs to be invoked after the modules have been loaded so that their loggers have been initialized.

    Args:
        - level: desired level. e.g. logging.INFO. Optional. Default is logging.ERROR
        - prefices: list of one or more str prefices to match (e.g. ["transformers", "torch"]). Optional.
          Default is `[""]` to match all active loggers.
          The match is a case-sensitive `module_name.startswith(prefix)`
    """
    prefix_re = re.compile(fr'^(?:{ "|".join(prefices) })')
    for name in logging.root.manager.loggerDict:
        if re.match(prefix_re, name):
            logging.getLogger(name).setLevel(level)
            
            
def query(payload, api_url, headers):
    data = json.dumps(payload)
    response = requests.request("POST", api_url, headers=headers, data=data)
    return json.loads(response.content.decode("utf-8"))


def format_results(query_output):
    res=pd.DataFrame()
    for index, x in enumerate(query_output):
        row=pd.DataFrame()
        for y in x:
            row=pd.concat([row, pd.DataFrame({y['label']: [y['score']]})], axis=1)
        row.insert(0, 'max', row.idxmax('columns'))
        res=pd.concat([res, row])
    return res


def chunk_sentence(sentence_list=[], tokenizer=None, max_length=0, rolling_window_perc=0.7):
    
    '''
    Split each sentence in sentence_list into chunks of max_length-6 with a rolling window of
    int(rolling_window_perc*max_length) so as to have a (1-rolling_window_perc)*max_length words' overlap
    between consecutive chunks. Each chunk is evaluated according to the corresponding tokens (may be more than
    total words in sentence) encoded by "tokenizer". Then tokens are decoded back to string so as to be ready
    to be processed by HuggingFace API, avoiding "Input is too long" error in the query.
    
    Args:
        - sentence_list: (list) list of sentence to be splitted
        - tokenizer: tokenizer such as BertTokenizer.from_pretrained()
        - max_length: (int) maximum number of token that can be processed by model
        - rolling_window_perc: (float in [0, 1]) percentage of "max_length" to be used as step for the rolling window.
                                (1-rolling_window_perc) will result into the number of words that overlap in each chunk.
    '''
    
    set_global_logging_level(logging.ERROR)   # silence warning for exceeding max_length in tokenizer

    reference_index=[]
    sentence_chunks=[]
    for index, txt in enumerate(sentence_list):

        print('Processing ' + str(index + 1) + ' / ' + str(len(sentence_list)), end = '\r')
        
        # tokenize
        tk=tokenizer.encode_plus(txt, add_special_tokens=False, return_tensors='pt')
        tokens=tk['input_ids'][0]

        # split into overlapping chunks
        max_ind=len(tokens)-1
        window_size=max_length-6
        window_step=int(rolling_window_perc*max_length)
        token_chunks_ind=[]
        i=0
        while True:
            cnk=list(range(i*window_step, min([i*window_step+window_size, len(tokens)])))
            token_chunks_ind.append(cnk)
            i+=1
            if max(cnk) == max_ind:
                break
        if np.in1d(np.array([[x[0], x[-1]] for x in token_chunks_ind]).ravel(), [0, max_ind]).sum() != 2: # check skipped start/end of sentence
            print(f'- Index {index}: token chunk index do not contain start and/or end of sentence')

        # decode back to sentence
        for ind in token_chunks_ind:
            sentence_chunks.append(tokenizer.decode(tokens[ind]))

        # store reference index to map each chunk to corresponding original sentence
        reference_index.extend([str(index).zfill(6)+'_'+str(i) for i in range(len(token_chunks_ind))]) # index_ChunkNum
        
    set_global_logging_level(logging.WARNING)
    if len(reference_index) != len(sentence_chunks):
        print('\n##### Warning: "reference_index" has different length from "sentence_chunks"')
    
    return reference_index, sentence_chunks


def sentence_classification(df_text, model_ID_list=[''], rolling_window_perc=0.7, query_batch_size=50, split_reload=False,
                            query_reload=False, cache_dir='', api_url='', headers=None, checkpoint_folder='', sentiment_folder=''):
    
    '''
    Evaluate text classification on sentences and returns class probabilities.
    
    Args:
        - model_ID_list: (list of str) HuggingFace model ID. E.g. 'nbroad/ESG-BERT'
        - rolling_window_perc: (float in [0, 1]) see chunk_sentence()
        - query_batch_size: (int) batch size of sentences to be sent to API
        - split_reload: (bool) if True reload the splitting of sentences into chunks by chunk_sentence()
        - cache_dir: (str) path for caching HuggingFace model (model will be downloaded if not found in the folder)
        - api_url: (str) url for HuggingFace API. Will be merged with model_ID
        - headers: (dict) headers used for the API to pass the API token
    '''

    if type(model_ID_list) != list:
        raise ValueError('"model_ID_list" must be a list of string')
    
    output={}
    for model_ID in model_ID_list:
        
        print('\n\n'+'#'*(70+len(model_ID)+4))
        print('#'+' '*35, model_ID, ' '*35+'#')
        print('#'*(70+len(model_ID)+4),'\n\n')
        model_lab=model_ID.replace('/','_')
    
        ### define model and tokenizer
        model = BertForSequenceClassification.from_pretrained(model_ID, cache_dir=cache_dir)
        tokenizer = BertTokenizer.from_pretrained(model_ID, do_lower_case=True, cache_dir=cache_dir)

        ### get prediction classe, special tokens and maximum sentence (in tokens) length
        # https://huggingface.co/transformers/v4.0.1/model_doc/bert.html?highlight=do_lower_case#berttokenizer

        pred_classes=model.config.id2label
        print('Prediction classes:')
        display(pd.DataFrame(pred_classes.values(), index=pred_classes.keys(), columns=['Class']))

        max_length = model.config.max_position_embeddings
        print('\nMaximum tokens allowed:', max_length)

        # get input_ids coding for "PAD". CLS will always be at the beginning, SEP at the end
        tk = tokenizer("[PAD]", truncation=False)
        token_CLS = tk['input_ids'][0]     # beginning of sentence, CLS for classification
        token_PAD = tk['input_ids'][1]
        token_SEP = tk['input_ids'][2]
        tk=tokenizer("[UNK]", truncation=False)
        token_UNK = tk['input_ids'][1]
        print('\nSpecial Tokens:\n[PAD]:', token_PAD , '\n[CLS]:', token_CLS, '\n[SEP]:', token_SEP, '\n[UNK]:', token_UNK)

        ### split the sentences into chunks
        print('\n\n-- Split sentences into chunks:\n')
        split_path=os.path.join(checkpoint_folder, '00_sentence_split_'+model_lab+'.pkl')
        if not split_reload or not os.path.exists(split_path):
            start=timer()
            reference_index, sentence_chunks = chunk_sentence(sentence_list=df_text['text_clean'],
                                                              tokenizer=tokenizer, max_length=max_length, rolling_window_perc=0.7)
            tot_time=str(datetime.timedelta(seconds=round(timer()-start)))
            joblib.dump({'reference_index': reference_index, 'sentence_chunks': sentence_chunks, 'tot_time': tot_time},
                        split_path, compress=('lzma', 3))
            print('\nDone in ', tot_time)
            print('Data saved in', split_path)
        else:
            rr=joblib.load(split_path)
            reference_index=rr['reference_index']
            sentence_chunks=rr['sentence_chunks']
            tot_time=rr['tot_time']
            print(f'Reloaded (evaluated in {tot_time})')
        print(f'\nTotal sentences: {len(df_text)}')
        print(f'Total chunked sentences: {len(reference_index)}')


        ### query from API
        print('\n\n-- Query from API:\n')
        chunk_ind=[list(range(len(reference_index)))[i:i + query_batch_size] for i in range(0, len(reference_index), query_batch_size)]
        query_log=pd.DataFrame()
        for i, ind in enumerate(chunk_ind):

            message=f'Querying batch ({query_batch_size} rows) {str(i + 1)} / {str(len(chunk_ind))}  last interaction: {datetime.datetime.now().strftime("%d/%m/%Y %H:%M:%S")}'
            print(message, end = '\r')

            chunck_path=os.path.join(sentiment_folder, 'sentence_query_'+model_lab+'_'+str(i)+'.pkl')
            if not query_reload or not os.path.exists(chunck_path):

                ref_ind=np.array(reference_index)[ind].tolist()
                text_batch=np.array(sentence_chunks)[ind].tolist()

                start=timer()
                max_try=1
                while max_try <= 2:
                    try:
                        query_out=query({"inputs": text_batch, "options": {"wait_for_model": True}},
                                        api_url=urljoin(api_url, model_ID), headers=headers)

                        if type(query_out)!=list and 'error' in query_out.keys():
                            query_out=pd.DataFrame({'Status': 'FAILED', 'Error': query_out['error'], 'ref_index': ref_ind})
                        else:
                            query_out=format_results(query_out)
                            query_out.insert(0, 'ref_index', ref_ind)
                            query_out.insert(0, 'Error', '')
                            query_out.insert(0, 'Status', 'OK')
                            break
                    except Exception as e:
                        query_out=pd.DataFrame({'Status': 'FAILED', 'Error': e, 'ref_index': ref_ind})
                    max_try+=1

                query_out.insert(0, 'Chunk', i)
                match_url=(pd.DataFrame([int(x.split('_')[0]) for x in ref_ind], columns=['index'])
                           .merge(df_text[['url']].reset_index(), on='index', how='left')).pop('url')
                query_out.insert(0, 'url', match_url.values)
                query_out['eval_time']=datetime.timedelta(seconds=round(timer()-start)).total_seconds()
                if len(query_out[query_out['Status']=='FAILED']) == 0:
                    query_out.to_pickle(chunck_path, protocol=-1)
            else:
                query_out=pd.read_pickle(chunck_path)

            query_log=pd.concat([query_log, query_out])
            query_path_csv=os.path.join(checkpoint_folder, '00_sentence_query_'+model_lab+'.csv')
            query_log.to_csv(query_path_csv, index=False, sep=';')
            tot_fail_batch=query_log[query_log['Status'] == 'FAILED']['Chunk'].nunique()
            tot_fail_rows=(query_log['Status'] == 'FAILED').sum()
            print(message+f'  - total failed batch: {tot_fail_batch} ({tot_fail_rows} rows)', end = '\r')
            
        query_log.insert(0, 'Model', model_ID)
        display(query_log['Status'].value_counts().to_frame())
        if len(query_log[query_log['Status'] == 'FAILED']) > 0:
            print(colored('Try to run the code again with', 'black', 'on_light_grey'),
                  'query_reload='+colored('True', 'green',  attrs=['bold']))
        output[model_lab]=query_log

        tot_time=query_log.groupby('Chunk').first()['eval_time'].sum()
        print('\nDone in ', str(datetime.timedelta(seconds=round(tot_time))))
        query_path=os.path.join(checkpoint_folder, '00_sentence_query_'+model_lab+'.pkl')
        query_log.to_pickle(query_path, protocol=-1)
        print('Data saved in', query_path)
        
    return output