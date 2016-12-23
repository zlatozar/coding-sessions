# -*- coding: utf-8 -*-

def pprint_tree(tree):

    if tree is None:
        return '<empty tree>'

    def recurse(node):

        if node is None:
            return [], 0, 0

        label = str(node.key)
        left_lines, left_pos, left_width = recurse(node.left)
        right_lines, right_pos, right_width = recurse(node.right)
        middle = max(right_pos + left_width - left_pos + 1, len(label), 2)

        pos = left_pos + middle // 2
        width = left_pos + middle + right_width - right_pos

        while len(left_lines) < len(right_lines):
            left_lines.append(' ' * left_width)

        while len(right_lines) < len(left_lines):
            right_lines.append(' ' * right_width)

        if (middle - len(label)) % 2 == 1 and node.parent is not None and \
                        node is node.parent.left and len(label) < middle:
            label += '.'

        label = label.center(middle, '.')
        if label[0] == '.':
            label = ' ' + label[1:]

        if label[-1] == '.':
            label = label[:-1] + ' '

        lines = [' ' * left_pos + label + ' ' * (right_width - right_pos),
                 ' ' * left_pos + '/' + ' ' * (middle-2) +
                 '\\' + ' ' * (right_width - right_pos)] + \
                [left_line + ' ' * (width - left_width - right_width) + right_line
                 for left_line, right_line in zip(left_lines, right_lines)]

        return lines, pos, width

    return '\n'.join(recurse(tree.root) [0])
