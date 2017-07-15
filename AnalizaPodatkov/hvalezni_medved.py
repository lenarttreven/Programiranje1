##########################################################################
# Hvaležni medved
#
# Pri tej nalogi bomo napisali nekaj funkcij, ki nam bodo v pomoč pri analizi
# literarnih besedil, kot je na primer koroška narodna pripovedka *Hvaležni
# medved*.
#
# Grateful Bear
#
# In this exercise we will write a few functions that help us analyse literary
# texts, such as the Carinthian folk tale "Grateful Bear".
#
##########################################################################

odlomek = """Gori nekje v gorah, ne ve se več, ali je bilo pri Macigoju ali
Naravniku, je šivala gospodinja v senci pod drevesom in zibala otroka. Naenkrat
prilomasti - pa prej ni ničesar opazila - medved in ji moli taco, v kateri je
tičal velik, debel trn. Žena se je prestrašila, a medved le milo in pohlevno
godrnja. Zato se žena ojunači in mu izdere trn iz tace. Mrcina kosmata pa zvrne
zibel, jo pobaše in oddide. Čez nekaj časa pa ji zopet prinese zibel, a zvhano
napolnjeno s sladkimi hruškami . Postavil jo je na tla pred začudeno mater in
odracal nazaj v goščavo. "Poglej no", se je razveselila mati, "kakšen hvaležen
medved. Zvrhano zibelko sladkih hrušk mi je prinesel za en sam izdrt trn"."""

##########################################################################
# 1) Sestavite funkcijo najdi_besede(besedilo, podniz), ki vrne množico
# vseh besed, ki se pojavijo v nizu besedilo in vsebujejo niz podniz.
# Zgled:
#
# >>> najdi_besede(odlomek, 'de')
# {'izdere', 'debel', 'oddide', 'začudeno'}
#
# 1) Write a function find_words(text, substring) that returns a set of all the
#    words in the text containing substring as substring.
#
# Example:
# >>> find_words(odlomek, 'de')
# {'izdere', 'debel', 'oddide', 'začudeno'}
##########################################################################
import re

def find_words(text, substring):
    r = re.compile(r'\b\w*{}\w*\b'.format(substring))
    return set(r.findall(text))



'''def najdi_besede(besedilo, podniz):
    razdeljeno_besedilo = set()
    trenutni_niz = ''
    for znak in besedilo:
        if znak.isalpha():
            trenutni_niz += znak
        elif trenutni_niz != '':
            razdeljeno_besedilo.add(trenutni_niz)
            trenutni_niz = ''
    if trenutni_niz != '':
        razdeljeno_besedilo.add(trenutni_niz)
    ujemanje = set()
    for beseda in razdeljeno_besedilo:
        if podniz in beseda:
            ujemanje.add(beseda)
    return ujemanje'''


##########################################################################
# 2) Sestavite funkcijo najdi_predpono(besedilo, predpona), ki vrne množico
# vseh besed, ki se pojavijo v nizu besedilo in imajo predpono predpona.
# Zgled:
#
# >>> najdi_predpono(odlomek, 'zi')
# {'zibala', 'zibel', 'zibelko'}
#
# 2) Write a function find_prefix(text, prefix) which returns the set of all
#    words in the text starting with prefix.
#
# Example:
# >>> find_prefix(odlomek, 'zi')
# {'zibala', 'zibel', 'zibelko'}
##########################################################################

def find_prefix(text, prefix):
    r = re.compile(r'\b{}\w*\b'.format(prefix))
    return set(r.findall(text))

'''def starts_with(string, prefix):
    if len(string) < len(prefix):
        return False
    else:
        for i in range(len(prefix)):
            if prefix[i] != string[i]:
                return False
    return True


def find_prefix(text, prefix):
    split_text = set()
    current_string = ''
    for character in text:
        if character.isalpha():
            current_string += character
        elif current_string != '':
            split_text.add(current_string)
            current_string = ''
    if current_string != '':
        split_text.add(current_string)
    matching = set()
    for word in split_text:
        if starts_with(word, prefix):
            matching.add(word)
    return matching

'''


##########################################################################
# 3) Sestavite funkcijo najdi_pripono(besedilo, pripona), ki vrne množico
# vseh besed, ki se pojavijo v nizu besedilo in imajo pripono pripona.
# Zgled:
#
# >>> najdi_pripono(odlomek, 'la')
# {'zibala', 'razveselila', 'prestrašila', 'šivala', 'opazila', 'tla'}
#
# 3) Write a function find_suffix(text, suffix) which returns the set of all
#    words in the text ending with suffix.
#
# Example:
# >>> find_suffix(odlomek, 'la')
# {'zibala', 'razveselila', 'prestrašila', 'šivala', 'opazila', 'tla'}
##########################################################################

def find_suffix(text, suffix):
    r = re.compile(r'\b\w*{}\b'.format(suffix))
    return set(r.findall(text))



'''def ends_with(word, suffix):
    if len(suffix) > len(word):
        return False
    else:
        word = word[::-1]
        suffix = suffix[::-1]
        for i in range(len(suffix)):
            if suffix[i] != word[i]:
                return False
    return True


def find_suffix(text, suffix):
    split_text = set()
    current_string = ''
    for character in text:
        if character.isalpha():
            current_string += character
        elif current_string != '':
            split_text.add(current_string)
            current_string = ''
    if current_string != '':
        split_text.add(current_string)
    matching = set()
    for word in split_text:
        if ends_with(word, suffix):
            matching.add(word)
    return matching
'''
##########################################################################
# 4) Sestavite funkcijo podvojene_crke(besedilo), ki sprejme niz besedilo
# in vrne množico vseh besed, ki vsebujejo podvojene črke. Zgled:
#
# >>> podvojene_crke('A volunteer is worth twenty pressed men.')
# {'pressed', 'volunteer'}
#
# 4) Write a function double_letters(text) that returns the set of words in
#    text that contain the same letter twice consecutively.
#
# Example:
# >>> double_letters('A volunteer is worth twenty pressed men.')
# {'volunteer', 'pressed'}
##########################################################################

def double_letters(text):
    r = re.compile(r'\b(\w*(\w)\2\w*)\b')
    return set(a[0] for a in r.findall(text))

'''def double_in_word(word):
    for i in range(len(word) - 1):
        if word[i] == word[i + 1]:
            return True
    return False

def double_letters(text):
    split_text = set()
    current_string = ''
    for character in text:
        if character.isalpha():
            current_string += character
        elif current_string != '':
            split_text.add(current_string)
            current_string = ''
    if current_string != '':
        split_text.add(current_string)
    matching = set()
    for word in split_text:
        if double_in_word(word):
            matching.add(word)
    return matching'''
