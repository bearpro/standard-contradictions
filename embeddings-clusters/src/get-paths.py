from xml.etree import ElementTree
from sys import argv
import re

def remove_ns(s):
    return re.sub(r'\{[^}]*\}', '', s)

def element_repr(element):
    repr_items = []
    clean_tag = remove_ns(element.tag)
    repr_items.append(clean_tag)

    if element.attrib:
        attribs_list = [f"{remove_ns(k)}={element.attrib[k]}" for k in element.attrib]
        attribs_str = ",".join(attribs_list)
        attribs = f"({attribs_str})"
        repr_items.append(attribs)

    if element.text and element.text.strip():
        clean_text = element.text.strip()
        formatted_text = f" = {clean_text}"
        repr_items.append(formatted_text)

    return "".join(repr_items)

def get_all_paths(element, path=None):
    """
    Рекурсивная функция для обхода XML-дерева в глубину.
    
    :param element: Текущий элемент XML-дерева.
    :param level: Текущий уровень вложенности (для форматированного вывода).
    """

    if path == None:
        path = []
    
    current_element = element_repr(element)
    next_path = path + [current_element]

    if len(element) == 0:
        return ["/".join(next_path)]
    else:
        results = []
        for child in element:
            results.extend(get_all_paths(child, next_path))
        return results

def main(argv: list[str]):
    try:
        reqif_path = argv[1]
    except Exception as e:
        raise Exception("first argument must be path to .reqif")
    
    reqif_root = ElementTree.parse(reqif_path).getroot()
    
    result = get_all_paths(reqif_root)
    for i in result:
        print(i)

if __name__ == "__main__":
    main(argv)
