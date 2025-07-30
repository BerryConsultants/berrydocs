#!/usr/bin/env python3
import sys
import shutil
import pathlib
import re

def copy_and_update(prev_tag: str, new_tag: str):
    '''
    Copy the entire directory documentation/<prev_tag>/ to documentation/<new_tag>/
    and update any internal .qmd links from <prev_tag>/... to <new_tag>/...
    Also create a release notes file under documentation/versions/v<major>/facts<new_tag*10>.qmd
    '''
    src_dir = pathlib.Path('documentation') / prev_tag
    dst_dir = pathlib.Path('documentation') / new_tag

    if not src_dir.exists() or not src_dir.is_dir():
        sys.exit(f'❌ Source folder {src_dir!r} does not exist or is not a directory')
    if dst_dir.exists():
        sys.exit(f'❌ Destination folder {dst_dir!r} already exists')

    # Copy the directory tree
    try:
        shutil.copytree(src_dir, dst_dir)
        print(f'✔️ Copied documentation from {src_dir!r} to {dst_dir!r}')
    except Exception as e:
        sys.exit(f'❌ Error copying documentation: {e}')

    # Pattern to find .qmd links with old tag
    link_pattern = re.compile(rf'{re.escape(prev_tag)}/[^)\s]+\.qmd')
    changes = []

    # Walk all .qmd files in the new directory
    for file in dst_dir.rglob('*.qmd'):
        text = file.read_text()
        matches = link_pattern.findall(text)
        if matches:
            new_text = text
            for old_link in matches:
                new_link = old_link.replace(f'{prev_tag}/', f'{new_tag}/')
                new_text = new_text.replace(old_link, new_link)
                changes.append((file, old_link, new_link))
            file.write_text(new_text)

    # Print summary of all changes
    if changes:
        print('✔️ Updated internal .qmd links:')
        for path, old, new in changes:
            print(f'  - {path!r}: {old} → {new}')
    else:
        print('ℹ️ No internal .qmd links to update in copied docs.')

    # Create release notes file
    versions_root = pathlib.Path('documentation/versions')
    n_str = new_tag.lstrip('v')
    n_int = int(n_str)
    major = n_int // 10
    minor = n_int % 10
    major_dir = versions_root / f'v{major}'
    try:
        major_dir.mkdir(parents=True, exist_ok=True)
    except Exception as e:
        sys.exit(f'❌ Could not create major version directory {major_dir!r}: {e}')

    release_filename = f'facts{n_int * 10}.qmd'
    release_file = major_dir / release_filename
    if release_file.exists():
        sys.exit(f'❌ Release notes file {release_file!r} already exists')

    title = f'FACTS {major}.{minor}.0 Release Notes'
    header = (
        '---\n'
        f'title: "{title}"\n'
        'subtitle: "XXX"\n'
        'format:\n'
        '  html:\n'
        '    toc: true\n'
        '    toc-depth: 3\n'
        '    toc-title: "Table of Contents"\n'
        '---\n'
    )
    try:
        release_file.write_text(header)
        print(f'✔️ Created release notes file {release_file!r}')
    except Exception as e:
        sys.exit(f'❌ Could not write release notes file {release_file!r}: {e}')


def main():
    if len(sys.argv) != 3:
        print('Usage: python3 copy_and_update_links.py <prev_version_tag> <new_version_tag>')
        sys.exit(1)
    prev_tag, new_tag = sys.argv[1], sys.argv[2]
    copy_and_update(prev_tag, new_tag)

if __name__ == '__main__':
    main()


