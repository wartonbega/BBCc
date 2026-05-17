
def levenstein(a, b):
    if min(len(a), len(b)) == 0:
        return max(len(a), len(b))
    elif a[0] == b[0]:
        return levenstein(a[1:], b[1:])
    else:
        return 1 + min(levenstein(a[1:], b), levenstein(a, b[1:]), levenstein(a[1:], b[1:]))
    

def main():
    a = "Totqefnlq zj"
    b = "Tolo qzqz el"
    print(levenstein(a, b))

if __name__ == "__main__":
    main()