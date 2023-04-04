import requests, threading

def send():    
    print(requests.get("http://localhost:5000").content)


send()
    
    
