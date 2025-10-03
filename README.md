# 小说阅读网

一个基于Haskell和Scotty框架构建的轻量级小说阅读网站。

## 功能特性

- 📚 小说列表展示
- 📖 章节阅读功能
- 🎨 响应式设计
- 🚀 轻量快速
- 💾 **SQLite数据库持久化存储**（已完成迁移）

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

### 小说 (Novel) - 数据库版本
```haskell
data Novel = Novel
    { novelId :: Int
    , title :: Text
    , author :: Text
    , description :: Text
    , createdAt :: Text
    }
```

### 章节 (Chapter) - 数据库版本
```haskell
data Chapter = Chapter
    { chapterId :: Int
    , chapterNovelId :: Int
    , chapterTitle :: Text
    , content :: Text
    , chapterCreatedAt :: Text
    }
```

## 数据库功能

### ✅ 已实现的数据库功能

- **SQLite数据库集成** - 完整的数据持久化支持
- **自动数据库初始化** - 首次运行自动创建数据库和表结构
- **示例数据插入** - 包含3本小说和6个章节的示例数据
- **CRUD操作** - 完整的增删改查功能
- **外键关联** - novels和chapters表的完整关联

### 数据库表结构

```sql
-- novels表
CREATE TABLE novels (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    author TEXT NOT NULL,
    description TEXT NOT NULL,
    createdAt DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- chapters表  
CREATE TABLE chapters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    novelId INTEGER NOT NULL,
    title TEXT NOT NULL,
    content TEXT NOT NULL,
    createdAt DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (novelId) REFERENCES novels(id)
);
```

## 扩展功能

### 添加更多小说

应用现在使用SQLite数据库存储数据。要添加新小说，可以通过以下方式：

1. **通过数据库管理工具**直接插入数据
2. **扩展应用功能**添加管理员界面
3. **使用API接口**（如果实现）

### 数据库管理

项目已完成SQLite数据库集成，具备以下功能：

- ✅ 自动数据库初始化
- ✅ 示例数据自动插入
- ✅ 完整CRUD操作
- ✅ 数据持久化存储

### 用户功能

可以添加用户注册、登录、书架等功能。

## 数据库操作示例

### 查询所有小说
```haskell
getAllNovels :: IO [Novel]
```

### 根据ID获取小说
```haskell
getNovelById :: Int -> IO (Maybe Novel)
```

### 获取小说章节列表
```haskell
getChaptersByNovelId :: Int -> IO [Chapter]
```

### 获取章节内容
```haskell
getChapterById :: Int -> IO (Maybe Chapter)
```

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
- [x] **SQLite数据库集成**（已完成）
- [ ] 小说数据导入/导出
- [ ] 用户数据备份
- [ ] 数据库备份和恢复功能

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
- **数据库**: SQLite (sqlite-simple库)
- **数据格式**: JSON (Aeson库)

## 开发说明

项目使用标准的Haskell项目结构：

```
followflee/
├── app/
│   └── Main.hs          # 主程序文件（包含数据库功能）
├── followflee.cabal     # 项目配置文件（包含SQLite依赖）
├── DATABASE_MIGRATION_SUMMARY.md  # 数据库迁移详细文档
├── CHANGELOG.md         # 项目变更日志
├── LICENSE              # 许可证文件
├── README.md            # 项目说明
└── followflee.db        # SQLite数据库文件（自动生成）
```

### 数据库相关文件

- `followflee.db` - SQLite数据库文件（首次运行自动生成）
- `DATABASE_MIGRATION_SUMMARY.md` - 数据库迁移完整文档
- `CHANGELOG.md` - 包含版本0.2.0.0的数据库迁移记录

## 许可证

BSD-3-Clause
