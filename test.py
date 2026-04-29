
def reduceDoubleSpace(s: str):
    ret = ""
    accu = True
    for c in s:
        if c == ' ' and accu:
            ret += c
            accu = False
        elif c != ' ':
            accu = True
            ret += c
    return  ret

def main():
    with open("test.bbc") as file:
        content = file.read()

    content = reduceDoubleSpace(content)
    for c in content: 
        if c != '\n' and c != '\t' and c != '\r':
            print(c, end="")

if __name__ == "__main__":
    main()