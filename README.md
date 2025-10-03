# 小说阅读网

一个基于Haskell和Scotty框架构建的轻量级小说阅读网站。

## 功能特性

- 📚 小说列表展示
- 📖 章节阅读功能
- 🎨 响应式设计
- 🚀 轻量快速
- 💾 **SQLite数据库持久化存储**（已完成迁移）
- 👤 **完整的用户系统**（注册、登录、会话管理）
- 📚 **个人书架功能**（添加、移除、查看收藏）
- 🔐 **密码加密安全**（BCrypt哈希加密）
- 🔑 **基于UUID令牌的会话管理**（真正的多用户支持）

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

### 页面路由
- `/` - 首页，展示推荐小说
- `/novels` - 小说列表页
- `/novel/:id` - 小说详情页（章节列表）
- `/chapter/:novelId/:chapterId` - 章节阅读页
- `/bookshelf` - 个人书架页（需要登录）
- `/about` - 关于页面

### 用户认证路由
- `/login` - 登录页面（基于UUID令牌的会话管理）
- `/register` - 注册页面
- `/logout` - 退出登录（清除会话令牌）

### 书架操作路由
- `/bookshelf/add/:novelId` - 添加到书架（POST）
- `/bookshelf/remove/:novelId` - 从书架移除（POST）

## 数据结构

### 用户 (User) - 数据库版本
```haskell
data User = User
    { userId :: Int
    , username :: Text
    , passwordHash :: Text
    , createdAt :: Text
    }
```

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

### 书架 (Bookshelf) - 数据库版本
```haskell
data Bookshelf = Bookshelf
    { bookshelfId :: Int
    , userId :: Int
    , novelId :: Int
    , addedAt :: Text
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
-- users表
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    passwordHash TEXT NOT NULL,
    createdAt DATETIME DEFAULT CURRENT_TIMESTAMP
);

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

-- bookshelf表
CREATE TABLE bookshelf (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    userId INTEGER NOT NULL,
    novelId INTEGER NOT NULL,
    addedAt DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (userId) REFERENCES users(id),
    FOREIGN KEY (novelId) REFERENCES novels(id),
    UNIQUE(userId, novelId)
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

## 会话管理重大修复

### 🔧 问题修复
原会话管理系统存在严重缺陷：所有用户共享同一个全局变量，导致所有连接者共享同一个账户状态。

### ✅ 解决方案
实现了基于UUID令牌的会话管理系统：
- **会话令牌**: 使用UUID.V4生成唯一会话令牌
- **会话存储**: 使用Data.Map存储令牌到用户ID的映射
- **Cookie管理**: 通过HTTP Cookie存储会话令牌，实现跨请求会话保持
- **安全特性**: 设置HttpOnly标志防止XSS攻击

### 🔑 核心功能
- `generateSessionToken` - 生成唯一会话令牌
- `getUserFromRequest` - 从Cookie中提取令牌并获取用户信息
- `setSessionToken` - 登录时设置会话令牌
- `clearSessionToken` - 退出登录时清除会话令牌

### 📈 修复效果
- ✅ **用户隔离**: 每个用户拥有独立的会话状态
- ✅ **会话持久化**: 登录状态通过Cookie保持
- ✅ **安全退出**: 退出登录时正确清除会话
- ✅ **防XSS**: HttpOnly Cookie保护
- ✅ **唯一令牌**: UUID确保会话令牌的唯一性

## 功能状态

### ✅ 已实现功能

#### 🔐 用户系统
- [x] 用户注册和登录功能
- [x] 密码加密和安全存储（BCrypt哈希加密）
- [x] **会话管理（基于UUID令牌的会话隔离）**
- [x] 退出登录功能

#### 📚 书架功能
- [x] 个人书架功能
- [x] 添加小说到书架
- [x] 从书架移除小说
- [x] 查看个人书架

#### 💾 数据持久化
- [x] SQLite数据库集成
- [x] 自动数据库初始化
- [x] 示例数据插入
- [x] 完整CRUD操作

#### 📖 小说阅读
- [x] 小说列表展示
- [x] 章节阅读功能
- [x] 响应式设计

### 🔄 待实现功能

#### 🔐 用户系统增强
- [ ] 用户个人资料管理
- [ ] 用户权限管理
- [ ] 密码重置功能

#### 📚 阅读体验优化
- [ ] 阅读进度保存
- [ ] 书签功能
- [ ] 夜间模式
- [ ] 字体大小调整
- [ ] 阅读背景色自定义

#### 🔍 搜索和筛选
- [ ] 小说搜索功能
- [ ] 按分类筛选小说
- [ ] 按作者筛选
- [ ] 热门小说排行

#### 📱 移动端优化
- [ ] PWA支持
- [ ] 离线阅读功能

#### 🔔 互动功能
- [ ] 评论系统
- [ ] 评分功能
- [ ] 小说推荐算法
- [ ] 阅读统计

#### 🛠️ 管理功能
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
- **密码加密**: BCrypt (用于用户密码安全存储)
- **会话管理**: UUID令牌 + Cookie存储 (真正的多用户会话隔离)
- **HTML表单处理**: Scotty的表单参数处理

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
