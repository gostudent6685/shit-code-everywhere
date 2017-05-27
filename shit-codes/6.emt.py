# Python, 2017-05-27
import json
import datetime


def getDate():
    return datetime.datetime.now().strftime("%Y-%m-%d")

def touch(filename):
    open(filename, "a").close()

def pop_all(the_list):
    for idx in range(len(the_list)):
        the_list.pop()

class ExpenseManager:
    def __init__(this):
        this.DBdata = []

    def load(this, filename = "emtdb.json"):
        this.filename = filename
        try:
            with open(filename, "r") as data:
                this.DBdata = json.loads(data.readline())
        except FileNotFoundError:
            touch(filename)
        except ValueError:
            pass
        this.showme()
        return this.DBdata

    def save(this):
        with open(this.filename, "w") as data:
            data.write(json.dumps(this.DBdata))

    def showme(this):
        if this.DBdata == []:
            print("No Data")
            return
        for each_content in this.DBdata:
            print(str(each_content))

    def sum(this):
        sum = 0
        for each_content in this.DBdata:
            sum = sum + each_content[0]
        return sum

    def format_content(value, comment):
        return [value, comment, getDate()]

    def merge(this):
        merged_value = this.sum()
        this.DBdata = [ExpenseManager.format_content(merged_value,"merged")]

    def spend(this, expense, comment):
        this.DBdata.append(ExpenseManager.format_content(-expense, comment))

    def get(this, amount, comment):
        this.DBdata.append(ExpenseManager.format_content(amount, comment))

    def pop(this):
        this.DBdata.pop()

def help_command():
    print("commands \n \
\n \
\nshowme \
\n    ; print data in nice form \
\nload [filename] \
\n    ; load data [from filename (defualt:emtdb.json)] (replace data) \
\nsave \
\n    ; save data (replace datafile) \
\nsum \
\n    ; get sum of expenses \
\nmerge \
\n    ; merge all expenses at one, whose value is equal to sum (replace data) \
\nspend expense comment \
\n    ; add tuple [-expense, comment] to list of expenses (replace data) \
\nget amount comment \
\n    ; add tuple [amount, comment] to list of expenses (replace data) \
\npop \
\n    ; delete most lately added item in the list of expenses (replace data) \
\nhelp \
\n    ; list whole commands and explanation \
\n? \
\n    ; list whole commands, no explanation \
\nquit \
\n    ; quit")

def command_list():
    print("showme load save sum merge spend get pop help ?");

def interprete_msg(emt, s):
    _s = s.split()
    quit = False
    try:
        if _s[0] == "showme":
            emt.showme()
        elif _s[0] == "?":
            command_list()
        elif _s[0] == "help":
            help_command()
        elif _s[0] == "spend":
            emt.spend(int(_s[1]),_s[2].strip("\""))
        elif _s[0] == "get":
            emt.get(int(_s[1]),_s[2].strip("\""))
        elif _s[0] == "load":
            if len(_s) == 1 or _s[1] == "":
                emt.load()
            else:
                emt.load(_s[1])
        elif _s[0] == "save":
            emt.save()
        elif _s[0] == "sum":
            print(str(emt.sum()))
        elif _s[0] == "merge":
            emt.merge()
        elif _s[0] == "q" or _s[0] == "quit":
            quit = True
        elif _s[0] == "pop":
            emt.pop()
        else:
            print("wrong input, try agin")
    except IndexError:
        print("wrong input, try again")
    return quit

if __name__=="__main__":
#def main_func():
    emt = ExpenseManager()
    quit = False
    while not quit:
        quit = interprete_msg(emt, input())