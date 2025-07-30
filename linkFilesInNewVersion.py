#!/usr/bin/env python3
import sys
import re
import copy
import pathlib
from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap, CommentedSeq

def bump_scalars(node, old_tag, new_tag):
    """
    Recursively replace old_tag→new_tag in all string keys and values.
    """
    if isinstance(node, CommentedMap):
        for key, val in list(node.items()):
            new_key = key.replace(old_tag, new_tag) if isinstance(key, str) else key
            if isinstance(val, (CommentedMap, CommentedSeq)):
                bump_scalars(val, old_tag, new_tag)
                new_val = val
            elif isinstance(val, str):
                new_val = val.replace(old_tag, new_tag)
            else:
                new_val = val
            if new_key != key:
                del node[key]
            node[new_key] = new_val
    elif isinstance(node, CommentedSeq):
        for i, item in enumerate(node):
            if isinstance(item, (CommentedMap, CommentedSeq)):
                bump_scalars(item, old_tag, new_tag)
            elif isinstance(item, str):
                node[i] = item.replace(old_tag, new_tag)


def update_index_qmd(prev_tag, new_tag):
    """
    Update links in documentation/index.qmd from prev_tag to new_tag.
    Prints each changed link.
    """
    index_path = pathlib.Path('documentation/index.qmd')
    if not index_path.exists():
        sys.exit(f"❌ `{index_path}` not found")
    text = index_path.read_text()
    pattern = re.compile(rf"{re.escape(prev_tag)}/[^)\s]+\.qmd")
    matches = pattern.findall(text)
    if not matches:
        print(f"ℹ️ No links to update in `{index_path}`.")
        return
    new_text = text
    for old in matches:
        new = old.replace(f"{prev_tag}/", f"{new_tag}/")
        new_text = new_text.replace(old, new)
        print(f"✔️  Updated link in `{index_path}`: `{old}` → `{new}`")
    index_path.write_text(new_text)


def update_sidebar(prev_tag, new_tag):
    """
    Insert new version section and link release notes in _quarto.yml sidebar,
    only if the version section doesn't already exist.
    """
    yaml = YAML()
    yaml.preserve_quotes = True
    conf = pathlib.Path('_quarto.yml')
    doc = yaml.load(conf)

    # human titles
    p, n = prev_tag.lstrip('v'), new_tag.lstrip('v')
    prev_title = f"Version {p[0]}.{p[1:]}"
    new_title = f"Version {n[0]}.{n[1:]}"

    # locate Documentation block
    sidebar = doc['website']['sidebar']
    for entry in sidebar:
        if entry.get('id') == 'Documentation':
            docs = entry
            break
    else:
        sys.exit("❌ Couldn’t find `id: Documentation` in sidebar")
    contents = docs['contents']

    # find old section index
    for idx, sec in enumerate(contents):
        if isinstance(sec, dict) and sec.get('section') == prev_title:
            old_idx = idx
            break
    else:
        sys.exit(f"❌ Couldn’t find `section: {prev_title}` under Documentation")

    # only insert new version section if not already present
    exists = any(isinstance(sec, dict) and sec.get('section') == new_title for sec in contents)
    if not exists:
        new_sec = copy.deepcopy(contents[old_idx])
        new_sec['section'] = new_title
        bump_scalars(new_sec, f"{prev_tag}/", f"{new_tag}/")
        contents.insert(old_idx, new_sec)
        print(f"✔️  Inserted `{new_title}` block into `_quarto.yml` sidebar")
    else:
        print(f"ℹ️ Section `{new_title}` already exists under Documentation, skipping insertion")

    # compute release notes path
    n_int = int(n)
    major = n_int // 10
    release_rel = f'documentation/versions/v{major}/facts{n_int*10}.qmd'

    # locate versions index block
    versions_entry = None
    for sec in contents:
        if isinstance(sec, dict) and sec.get('section') == 'documentation/versions/index.qmd':
            versions_entry = sec
            break
    if not versions_entry:
        sys.exit("❌ Couldn’t find `section: documentation/versions/index.qmd` under Documentation")
    v_contents = versions_entry['contents']

    # find or create major release notes section
    major_title = f'FACTS {major} Release Notes'
    major_sec = None
    for sec in v_contents:
        if isinstance(sec, dict) and sec.get('section') == major_title:
            major_sec = sec
            break
    if not major_sec:
        major_sec = CommentedMap()
        major_sec['section'] = major_title
        major_sec['contents'] = CommentedSeq()
        # insert before all existing sections
        v_contents.insert(0, major_sec)
        print(f"✔️  Created sidebar section `{major_title}`")

    # append release notes link if missing
    if release_rel not in major_sec['contents']:
        major_sec['contents'].append(release_rel)
        print(f"✔️  Linked release notes `{release_rel}` in `{major_title}` section")

    # write updated config
    with open(conf, 'w') as f:
        yaml.dump(doc, f)


def main():
    if len(sys.argv) != 3:
        print('Usage: python3 update_index_and_sidebar.py <prev_version_tag> <new_version_tag>')
        sys.exit(1)
    prev_tag, new_tag = sys.argv[1], sys.argv[2]
    update_index_qmd(prev_tag, new_tag)
    update_sidebar(prev_tag, new_tag)

if __name__ == '__main__':
    main()
