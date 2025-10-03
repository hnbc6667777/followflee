# 小说阅读网

一个基于Haskell和Scotty框架构建的轻量级小说阅读网站。

## 功能特性

- 📚 小说列表展示
- 📖 章节阅读功能
- 🎨 响应式设计
- 🚀 轻量快速
- 💾 内存数据存储（可扩展为数据库）

## 安装和运行

### 前置要求

- Haskell Stack 或 Cabal
- GHC 8.10.7 或更高版本

### 安装依赖

```bash
# 使用Cabal
cabal update
cabal build

# 或使用Stack
stack build
```

### 运行网站

```bash
# 使用Cabal
cabal run

# 或使用Stack
stack run
```

网站将在 http://localhost:3000 启动

## 网站结构

- `/` - 首页，展示推荐小说
- `/novels` - 小说列表页
- `/novel/:id` - 小说详情页（章节列表）
- `/chapter/:novelId/:chapterId` - 章节阅读页
- `/about` - 关于页面

## 数据结构

### 小说 (Novel)
```haskell
data Novel = Novel
    { novelId :: Int
    , title :: Text
    , author :: Text
    , description :: Text
    , chapters :: [Chapter]
    }
```

### 章节 (Chapter)
```haskell
data Chapter = Chapter
    { chapterId :: Int
    , chapterTitle :: Text
    , content :: Text
    }
```

## 扩展功能

### 添加更多小说

在 `novels` 列表中添加新的小说数据：

```haskell
novels =
    [ -- 现有小说...
    , Novel 4 "新小说标题" "作者名" "小说描述"
        [ Chapter 1 "第一章标题" "章节内容..."
        , Chapter 2 "第二章标题" "章节内容..."
        ]
    ]
```

### 连接数据库

当前使用内存数据，可以扩展为连接数据库：

1. 添加数据库依赖（如postgresql-simple）
2. 创建数据库表结构
3. 实现数据库操作函数

### 用户功能

可以添加用户注册、登录、书架等功能。

## 待实现功能

以下是网站当前版本尚未实现但计划在未来版本中添加的功能：

### 🔐 用户系统
- [ ] 用户注册和登录功能
- [ ] 用户个人资料管理
- [ ] 密码加密和安全存储
- [ ] 用户权限管理

### 📚 阅读体验优化
- [ ] 阅读进度保存
- [ ] 书签功能
- [ ] 夜间模式
- [ ] 字体大小调整
- [ ] 阅读背景色自定义

### 🔍 搜索和筛选
- [ ] 小说搜索功能
- [ ] 按分类筛选小说
- [ ] 按作者筛选
- [ ] 热门小说排行

### 💾 数据持久化
- [ ] 数据库集成（PostgreSQL/MySQL）
- [ ] 小说数据导入/导出
- [ ] 用户数据备份

### 📱 移动端优化
- [ ] 响应式移动端界面
- [ ] PWA支持
- [ ] 离线阅读功能

### 🔔 互动功能
- [ ] 评论系统
- [ ] 评分功能
- [ ] 小说推荐算法
- [ ] 阅读统计

### 🛠️ 管理功能
- [ ] 管理员后台
- [ ] 小说内容管理
- [ ] 用户管理
- [ ] 系统监控

## 技术栈

- **框架**: Scotty (Haskell Web框架)
- **模板**: Blaze-html (HTML DSL)
- **服务器**: Warp (高性能Web服务器)
- **数据格式**: JSON (Aeson库)

## 开发说明

项目使用标准的Haskell项目结构：

```
followflee/
├── app/
│   └── Main.hs          # 主程序文件
├── followflee.cabal     # 项目配置文件
├── LICENSE              # 许可证文件
└── README.md            # 项目说明
```

## 许可证

BSD-3-Clause
