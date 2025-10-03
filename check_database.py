#!/usr/bin/env python3
import sqlite3

def check_database():
    try:
        # 连接到数据库
        conn = sqlite3.connect('followflee.db')
        cursor = conn.cursor()
        
        # 查询所有表
        cursor.execute('SELECT name FROM sqlite_master WHERE type="table"')
        tables = cursor.fetchall()
        print('数据库中的表:')
        for table in tables:
            print(f'- {table[0]}')
        
        # 查询novels表数据
        cursor.execute('SELECT * FROM novels')
        novels = cursor.fetchall()
        print('\nnovels表数据:')
        for novel in novels:
            print(f'ID: {novel[0]}, 标题: {novel[1]}, 作者: {novel[2]}')
        
        # 查询chapters表数据
        cursor.execute('SELECT * FROM chapters')
        chapters = cursor.fetchall()
        print('\nchapters表数据:')
        for chapter in chapters:
            print(f'ID: {chapter[0]}, 小说ID: {chapter[1]}, 章节号: {chapter[2]}, 标题: {chapter[3]}')
        
        conn.close()
        print('\n数据库查询完成！')
        
    except Exception as e:
        print(f'查询数据库时出错: {e}')

if __name__ == "__main__":
    check_database()