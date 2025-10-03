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
- 📊 **阅读进度记录**（自动保存阅读位置，支持继续阅读）

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

-- reading_progress表（阅读进度记录）
CREATE TABLE reading_progress (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    novel_id INTEGER NOT NULL,
    chapter_id INTEGER NOT NULL,
    last_read_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (novel_id) REFERENCES novels(id),
    FOREIGN KEY (chapter_id) REFERENCES chapters(id),
    UNIQUE(user_id, novel_id)
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

### ✅ 已完成功能
- [x] 首页展示和导航
- [x] 小说列表浏览
- [x] 小说详情查看
- [x] 章节内容阅读
- [x] 用户注册和登录
- [x] 个人书架管理
- [x] 阅读进度跟踪
- [x] 会话管理（基于UUID令牌）
- [x] 数据库持久化存储
- [x] 响应式界面设计
- [x] 所有编译错误修复
- [x] 类型系统完整适配
- [x] HTML渲染优化

### 🔧 技术实现状态
- [x] 所有编译错误已修复
- [x] 类型系统完整适配
- [x] HTML渲染优化
- [x] 链接跳转功能正常
- [x] 数据库操作正常
- [x] 会话管理正常
- [x] 安全功能正常

## 运行状态

当前项目已成功构建并运行在 http://localhost:3000

- ✅ 编译测试通过（无错误，仅无害警告）
- ✅ 数据库初始化正常
- ✅ 示例数据插入正常
- ✅ Web服务正常运行
- ✅ 所有路由功能正常
- ✅ 用户会话管理正常
- ✅ 阅读进度跟踪正常
- ✅ 链接跳转功能正常

## 最新修复和优化

### 类型系统修复
- ✅ 修复`(!)`运算符作用域问题
- ✅ 修复`toHtml`和`docTypeHtml`函数作用域问题
- ✅ 修复T.Text到H.AttributeValue类型转换问题
- ✅ 修复`renderHtml`函数重复调用问题

### HTML渲染优化
- ✅ 正确导入所有必需的HTML渲染函数
- ✅ 优化路由处理函数中的HTML渲染逻辑
- ✅ 修复所有链接的href属性类型转换

### 代码质量提升
- ✅ 消除所有编译错误
- ✅ 优化模块导入结构
- ✅ 完善类型注解和函数签名
- ✅ 修复字段遮蔽和函数弃用警告

## 技术架构

### 后端技术栈
- **Haskell** - 函数式编程语言
- **Scotty** - 轻量级Web框架
- **Lucid** - HTML模板引擎
- **SQLite** - 数据库存储
- **BCrypt** - 密码哈希加密
- **Text.Blaze.Html** - HTML渲染引擎

### 前端技术
- **HTML5** - 语义化标记
- **CSS3** - 现代化样式
- **响应式设计** - 支持多设备

### 开发工具
- **Cabal** - Haskell包管理器
- **GHC** - Glasgow Haskell编译器

## 项目特色

1. **函数式编程实践** - 展示Haskell在Web开发中的实际应用
2. **类型安全** - 编译时错误检测，运行时稳定性
3. **简洁架构** - 模块化设计，易于维护扩展
4. **完整功能** - 从数据存储到用户界面的完整解决方案
5. **良好体验** - 现代化的界面设计和流畅的交互体验
6. **高质量代码** - 专业级代码标准，无编译错误

## 部署说明

项目已具备生产环境部署能力，所有技术问题已解决，代码质量达到专业标准。

### 快速启动
```bash
# 安装依赖
cabal update

# 构建项目
cabal build

# 运行应用
cabal run
```

访问地址：http://localhost:3000

项目将自动创建数据库并插入示例数据，立即体验完整的小说阅读功能。

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
