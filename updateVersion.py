#!/usr/bin/env python3
import sys, re, copy, shutil
from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap, CommentedSeq
import pathlib

def bump_scalars(node, old_tag, new_tag):
    """
    Recursively walk a ruamel.yaml node (CommentedMap or CommentedSeq),
    replacing old_tag→new_tag in ALL string keys and values.
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


def main():
    if len(sys.argv) != 3:
        print("Usage: python3 updateVersion.py <prev_version_tag> <new_version_tag>")
        sys.exit(1)

    prev_tag, new_tag = sys.argv[1], sys.argv[2]
    # build human titles "7.1" from "v71"
    p, n = prev_tag.lstrip("v"), new_tag.lstrip("v")
    prev_title = f"Version {p[0]}.{p[1:]}"
    new_title  = f"Version {n[0]}.{n[1:]}"

    yaml = YAML()
    yaml.preserve_quotes = True
    doc = yaml.load(open("_quarto.yml"))

    # locate the Documentation block in website → sidebar
    sidebar = doc["website"]["sidebar"]
    for entry in sidebar:
        if entry.get("id") == "Documentation":
            docs = entry
            break
    else:
        sys.exit("❌ Couldn’t find `id: Documentation` under website.sidebar")

    cont = docs["contents"]

    # find the old-version section
    for idx, sec in enumerate(cont):
        if isinstance(sec, dict) and sec.get("section") == prev_title:
            old_idx = idx
            break
    else:
        sys.exit(f"❌ Couldn’t find a `section: {prev_title}` under Documentation")

    # copy, relabel, and bump every path in the copy
    new_sec = copy.deepcopy(cont[old_idx])
    new_sec["section"] = new_title
    bump_scalars(new_sec, prev_tag + "/", new_tag + "/")

    # insert the new-version block
    cont.insert(old_idx, new_sec)

    # write back to YAML
    with open("_quarto.yml", "w") as f:
        yaml.dump(doc, f)
    print(f"✔️  Inserted `{new_title}` block into `_quarto.yml`")

    # regex-patch the index QMD
    index_qmd = pathlib.Path("documentation/index.qmd")
    text = index_qmd.read_text()
    # find and replace old-version paths
    index_pattern = rf"{re.escape(prev_tag)}/"
    if re.search(index_pattern, text):
        new_text = re.sub(index_pattern, f"{new_tag}/", text)
        # record changes for printing
        old_links = re.findall(rf"{re.escape(prev_tag)}/[^)\s]+\.qmd", text)
        index_qmd.write_text(new_text)
        for old_link in old_links:
            new_link = old_link.replace(prev_tag + "/", new_tag + "/")
            print(f"✔️  Updated link in `{index_qmd}`: `{old_link}` → `{new_link}`")
    print(f"✔️  Updated `documentation/index.qmd` → all paths now use `{new_tag}/`")

    # copy documentation directory contents from old to new
    src_dir = pathlib.Path("documentation") / prev_tag
    dst_dir = pathlib.Path("documentation") / new_tag
    if dst_dir.exists():
        sys.exit(f"❌ Destination folder `{dst_dir}` already exists")
    try:
        shutil.copytree(src_dir, dst_dir)
        print(f"✔️  Copied documentation from `{src_dir}` to `{dst_dir}`")
    except Exception as e:
        sys.exit(f"❌ Error copying documentation: {e}")

    # update internal links in newly copied QMD files
    qmd_files = list(dst_dir.rglob("*.qmd"))
    changed = []
    link_pattern = rf"{re.escape(prev_tag)}/[^)\s]+\.qmd"
    for file in qmd_files:
        text = file.read_text()
        # find all old-version .qmd links
        matches = re.findall(link_pattern, text)
        if matches:
            new_text = text
            for old_link in matches:
                new_link = old_link.replace(prev_tag + "/", new_tag + "/")
                new_text = new_text.replace(old_link, new_link)
                changed.append((file, old_link, new_link))
            file.write_text(new_text)
    # print all changes
    for file, old_link, new_link in changed:
        print(f"✔️  Updated link in `{file}`: `{old_link}` → `{new_link}`")

if __name__ == "__main__":
    main()

