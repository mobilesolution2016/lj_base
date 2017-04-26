--base code from: https://github.com/pguillory/luajit-gumbo
--thanks!

local ffi = require('ffi')

ffi.cdef[[
typedef struct {
	unsigned int line;
	unsigned int column;
	unsigned int offset;
} GumboSourcePosition;

extern const GumboSourcePosition kGumboEmptySourcePosition;

typedef struct {
	const char* data;
	size_t length;
} GumboStringPiece;

extern const GumboStringPiece kGumboEmptyString;

bool gumbo_string_equals(const GumboStringPiece* str1, const GumboStringPiece* str2);
bool gumbo_string_equals_ignore_case(const GumboStringPiece* str1, const GumboStringPiece* str2);

typedef struct {
	void** data;
	unsigned int length;
	unsigned int capacity;
} GumboVector;

extern const GumboVector kGumboEmptyVector;
int gumbo_vector_index_of(GumboVector* vector, const void* element);

typedef enum {
	GUMBO_TAG_HTML,	GUMBO_TAG_HEAD,	GUMBO_TAG_TITLE, GUMBO_TAG_BASE, GUMBO_TAG_LINK, GUMBO_TAG_META,
	GUMBO_TAG_STYLE, GUMBO_TAG_SCRIPT, GUMBO_TAG_NOSCRIPT, GUMBO_TAG_TEMPLATE, GUMBO_TAG_BODY, GUMBO_TAG_ARTICLE,
	GUMBO_TAG_SECTION, GUMBO_TAG_NAV, GUMBO_TAG_ASIDE, GUMBO_TAG_H1, GUMBO_TAG_H2, GUMBO_TAG_H3, GUMBO_TAG_H4,
	GUMBO_TAG_H5, GUMBO_TAG_H6, GUMBO_TAG_HGROUP, GUMBO_TAG_HEADER, GUMBO_TAG_FOOTER, GUMBO_TAG_ADDRESS,
	GUMBO_TAG_P, GUMBO_TAG_HR, GUMBO_TAG_PRE, GUMBO_TAG_BLOCKQUOTE, GUMBO_TAG_OL, GUMBO_TAG_UL, GUMBO_TAG_LI,
	GUMBO_TAG_DL, GUMBO_TAG_DT, GUMBO_TAG_DD, GUMBO_TAG_FIGURE, GUMBO_TAG_FIGCAPTION, GUMBO_TAG_MAIN,
	GUMBO_TAG_DIV, GUMBO_TAG_A, GUMBO_TAG_EM, GUMBO_TAG_STRONG, GUMBO_TAG_SMALL, GUMBO_TAG_S, GUMBO_TAG_CITE,
	GUMBO_TAG_Q, GUMBO_TAG_DFN, GUMBO_TAG_ABBR, GUMBO_TAG_DATA, GUMBO_TAG_TIME, GUMBO_TAG_CODE, GUMBO_TAG_VAR,
	GUMBO_TAG_SAMP, GUMBO_TAG_KBD, GUMBO_TAG_SUB, GUMBO_TAG_SUP, GUMBO_TAG_I, GUMBO_TAG_B, GUMBO_TAG_U,
	GUMBO_TAG_MARK, GUMBO_TAG_RUBY, GUMBO_TAG_RT, GUMBO_TAG_RP, GUMBO_TAG_BDI, GUMBO_TAG_BDO, GUMBO_TAG_SPAN,
	GUMBO_TAG_BR, GUMBO_TAG_WBR, GUMBO_TAG_INS, GUMBO_TAG_DEL, GUMBO_TAG_IMAGE, GUMBO_TAG_IMG, GUMBO_TAG_IFRAME,
	GUMBO_TAG_EMBED, GUMBO_TAG_OBJECT, GUMBO_TAG_PARAM, GUMBO_TAG_VIDEO, GUMBO_TAG_AUDIO, GUMBO_TAG_SOURCE,
	GUMBO_TAG_TRACK, GUMBO_TAG_CANVAS, GUMBO_TAG_MAP, GUMBO_TAG_AREA, GUMBO_TAG_MATH, GUMBO_TAG_MI,
	GUMBO_TAG_MO, GUMBO_TAG_MN, GUMBO_TAG_MS, GUMBO_TAG_MTEXT, GUMBO_TAG_MGLYPH, GUMBO_TAG_MALIGNMARK,
	GUMBO_TAG_ANNOTATION_XML, GUMBO_TAG_SVG, GUMBO_TAG_FOREIGNOBJECT, GUMBO_TAG_DESC, GUMBO_TAG_TABLE,
	GUMBO_TAG_CAPTION, GUMBO_TAG_COLGROUP, GUMBO_TAG_COL, GUMBO_TAG_TBODY, GUMBO_TAG_THEAD, GUMBO_TAG_TFOOT,
	GUMBO_TAG_TR, GUMBO_TAG_TD, GUMBO_TAG_TH, GUMBO_TAG_FORM, GUMBO_TAG_FIELDSET, GUMBO_TAG_LEGEND, GUMBO_TAG_LABEL,
	GUMBO_TAG_INPUT, GUMBO_TAG_BUTTON, GUMBO_TAG_SELECT, GUMBO_TAG_DATALIST, GUMBO_TAG_OPTGROUP,
	GUMBO_TAG_OPTION, GUMBO_TAG_TEXTAREA, GUMBO_TAG_KEYGEN, GUMBO_TAG_OUTPUT, GUMBO_TAG_PROGRESS,
	GUMBO_TAG_METER, GUMBO_TAG_DETAILS, GUMBO_TAG_SUMMARY, GUMBO_TAG_MENU, GUMBO_TAG_MENUITEM,
	GUMBO_TAG_APPLET, GUMBO_TAG_ACRONYM, GUMBO_TAG_BGSOUND, GUMBO_TAG_DIR, GUMBO_TAG_FRAME,
	GUMBO_TAG_FRAMESET, GUMBO_TAG_NOFRAMES, GUMBO_TAG_ISINDEX, GUMBO_TAG_LISTING, GUMBO_TAG_XMP,
	GUMBO_TAG_NEXTID, GUMBO_TAG_NOEMBED, GUMBO_TAG_PLAINTEXT, GUMBO_TAG_RB, GUMBO_TAG_STRIKE, GUMBO_TAG_BASEFONT,
	GUMBO_TAG_BIG, GUMBO_TAG_BLINK, GUMBO_TAG_CENTER, GUMBO_TAG_FONT, GUMBO_TAG_MARQUEE, GUMBO_TAG_MULTICOL,
	GUMBO_TAG_NOBR, GUMBO_TAG_SPACER, GUMBO_TAG_TT, GUMBO_TAG_RTC,

	GUMBO_TAG_UNKNOWN,
	GUMBO_TAG_LAST,
} GumboTag;

const char* gumbo_normalized_tagname(GumboTag tag);
void gumbo_tag_from_original_text(GumboStringPiece* text);
const char* gumbo_normalize_svg_tagname(const GumboStringPiece* tagname);

GumboTag gumbo_tag_enum(const char* tagname);
GumboTag gumbo_tagn_enum(const char* tagname, unsigned int length);

typedef enum {
	GUMBO_ATTR_NAMESPACE_NONE,
	GUMBO_ATTR_NAMESPACE_XLINK,
	GUMBO_ATTR_NAMESPACE_XML,
	GUMBO_ATTR_NAMESPACE_XMLNS,
} GumboAttributeNamespaceEnum;

typedef struct {
	GumboAttributeNamespaceEnum attr_namespace;
	const char* name;
	GumboStringPiece original_name;
	const char* value;
	GumboStringPiece original_value;
	GumboSourcePosition name_start;
	GumboSourcePosition name_end;
	GumboSourcePosition value_start;
	GumboSourcePosition value_end;
} GumboAttribute;

GumboAttribute* gumbo_get_attribute(const GumboVector* attrs, const char* name);

typedef enum {
	GUMBO_NODE_DOCUMENT,
	GUMBO_NODE_ELEMENT,
	GUMBO_NODE_TEXT,
	GUMBO_NODE_CDATA,
	GUMBO_NODE_COMMENT,
	GUMBO_NODE_WHITESPACE,
	GUMBO_NODE_TEMPLATE
} GumboNodeType;

typedef struct GumboInternalNode GumboNode;
typedef enum {
	GUMBO_DOCTYPE_NO_QUIRKS,
	GUMBO_DOCTYPE_QUIRKS,
	GUMBO_DOCTYPE_LIMITED_QUIRKS
} GumboQuirksModeEnum;

typedef enum {
	GUMBO_NAMESPACE_HTML,
	GUMBO_NAMESPACE_SVG,
	GUMBO_NAMESPACE_MATHML
} GumboNamespaceEnum;

typedef enum {
	GUMBO_INSERTION_NORMAL = 0,
	GUMBO_INSERTION_BY_PARSER = 1 << 0,
	GUMBO_INSERTION_IMPLICIT_END_TAG = 1 << 1,
	GUMBO_INSERTION_IMPLIED = 1 << 3,
	GUMBO_INSERTION_CONVERTED_FROM_END_TAG = 1 << 4,
	GUMBO_INSERTION_FROM_ISINDEX = 1 << 5,
	GUMBO_INSERTION_FROM_IMAGE = 1 << 6,
	GUMBO_INSERTION_RECONSTRUCTED_FORMATTING_ELEMENT = 1 << 7,
	GUMBO_INSERTION_ADOPTION_AGENCY_CLONED = 1 << 8,
	GUMBO_INSERTION_ADOPTION_AGENCY_MOVED = 1 << 9,
	GUMBO_INSERTION_FOSTER_PARENTED = 1 << 10,
} GumboParseFlags;

typedef struct {
	GumboVector children;
	bool has_doctype;
	const char* name;
	const char* public_identifier;
	const char* system_identifier;

	GumboQuirksModeEnum doc_type_quirks_mode;
} GumboDocument;

typedef struct {
	const char* text;
	GumboStringPiece original_text;
	GumboSourcePosition start_pos;
} GumboText;

typedef struct {
	GumboVector children;
	GumboTag tag;
	GumboNamespaceEnum tag_namespace;
	GumboStringPiece original_tag;
	GumboStringPiece original_end_tag;
	GumboSourcePosition start_pos;
	GumboSourcePosition end_pos;
	GumboVector attributes;
} GumboElement;

struct GumboInternalNode {
	GumboNodeType type;
	GumboNode* parent;
	size_t index_within_parent;
	GumboParseFlags parse_flags;
	union {
		GumboDocument document;
		GumboElement element;
		GumboText text;
	} v;
};

typedef void* (*GumboAllocatorFunction)(void* userdata, size_t size);
typedef void (*GumboDeallocatorFunction)(void* userdata, void* ptr);
typedef struct GumboInternalOptions {
	GumboAllocatorFunction allocator;
	GumboDeallocatorFunction deallocator;
	void* userdata;
	int tab_stop;
	bool stop_on_first_error;
	int max_errors;
	GumboTag fragment_context;
	GumboNamespaceEnum fragment_namespace;
} GumboOptions;

extern const GumboOptions kGumboDefaultOptions;

typedef struct GumboInternalOutput {
	GumboNode* document;
	GumboNode* root;
	GumboVector errors;
} GumboOutput;

GumboOutput* gumbo_parse(const char* buffer);
GumboOutput* gumbo_parse_with_options(const GumboOptions* options, const char* buffer, size_t buffer_length);
void gumbo_destroy_output(const GumboOptions* options, GumboOutput* output);
]]

local libgumbo = (function()
	local filename
	if ffi.os == 'Windows' then
		filename = package.searchpath('.\\gumbo', '?.dll', '')
	else
		filename = package.searchpath('./libgumbo', '?.dylib;?.so;', '')
	end

	assert(filename, 'libgumbo')
	return ffi.load(filename)
end)()

assert(libgumbo)

local GUMBO_NODE_DOCUMENT	= tonumber(libgumbo.GUMBO_NODE_DOCUMENT)
local GUMBO_NODE_ELEMENT	= tonumber(libgumbo.GUMBO_NODE_ELEMENT)
local GUMBO_NODE_TEXT		= tonumber(libgumbo.GUMBO_NODE_TEXT)
local GUMBO_NODE_CDATA		= tonumber(libgumbo.GUMBO_NODE_CDATA)
local GUMBO_NODE_COMMENT	= tonumber(libgumbo.GUMBO_NODE_COMMENT)
local GUMBO_NODE_WHITESPACE = tonumber(libgumbo.GUMBO_NODE_WHITESPACE)

local function transform_gumbo_node_to_lom_node(gumbo_node, luaQuery, level)
	local node_type = tonumber(gumbo_node.type)

	if node_type == GUMBO_NODE_DOCUMENT then
		error('Not implemented')
	elseif node_type == GUMBO_NODE_ELEMENT then
		local element = gumbo_node.v.element
		local attributes = ffi.cast('GumboAttribute**', element.attributes.data)
		local children = ffi.cast('GumboNode**', element.children.data)

		local node = {}
		node.tag = ffi.string(libgumbo.gumbo_normalized_tagname(element.tag))
		if node.tag == '' then
			libgumbo.gumbo_tag_from_original_text(element.original_tag)
			node.tag = ffi.string(element.original_tag.data, element.original_tag.length)
		end

		node.attr = {}
		for i = 0, element.attributes.length - 1 do
			local attribute = attributes[i]
			local name = ffi.string(attribute.name)
			local value = ffi.string(attribute.value)
			node.attr[name] = value
			table.insert(node.attr, name)
		end

		if luaQuery then
			--按class归类
			local names = node.attr.class
			if names then
				names = string.split(names, ' ')
				for i = 1, #names do
					local n = names[i]
					local c = luaQuery.byClasses[n]
					if not c then
						c = {}
						luaQuery.byClasses[n] = c
					end

					c[#c + 1] = node
				end
			end

			--按TagName归类
			local c = luaQuery.byTagNames[node.tag]
			if not c then
				c = {}
				luaQuery.byTagNames[node.tag] = c
			end

			c[#c + 1] = node

			--按nodename归类
			names = node.attr.name
			if names then
				local c = luaQuery.byNames[node.tag]
				if not c then
					c = {}
					luaQuery.byNames[node.tag] = c
				end

				c[#c + 1] = node
			end

			--按id索引
			names = node.attr.id
			if names then
				luaQuery.byIds[names] = node
			end
		end

		for i = 0, element.children.length - 1 do
			local childNode = transform_gumbo_node_to_lom_node(children[i], luaQuery, level + 1)
			if childNode then
				childNode.parent = node
				table.insert(node, childNode)
			end
		end

		return node

	elseif node_type == GUMBO_NODE_COMMENT then
		return nil
	else
		return { tag = '#text', text = ffi.string(gumbo_node.v.text.text) }
	end
end

--------------------------------------------------------------------------------
-- do like jQuery
--------------------------------------------------------------------------------

local gumbo = table.new(0, 4)

local COLLECT_DIRECTCHILDREN = 1
local COLLECT_ALLCHILDREN = 2

local BREAK_NONE = 0
local BREAK_ONE = 1
local BREAK_ALL = 2

--仅收集直系子节点
local function collectDirectChildren(sets, parent, nodes, collector)
	if type(nodes) == 'table' then
		local nd, brk = nil, BREAK_NONE
		for i = 1, #nodes do
			nd = nodes[i]
			if not parent or nd.parent == parent then
				brk = collector(sets, nodes, nd, i)
				if brk > BREAK_NONE then
					return brk
				end
			end
		end
	end
end
--收集所有子节点只要其父节点是自己的
local function collectAllChildren(sets, parent, nodes, collector)
	if type(nodes) == 'table' then
		local nd, brk = nil, BREAK_NONE
		for i = 1, #nodes do
			nd = nodes[i]
			while nd do
				if not parent or nd.parent == parent then
					brk = collector(sets, nodes, nodes[i], i)
					if brk > BREAK_NONE then
						return brk
					end
					break
				end
				nd = nd.parent
			end
		end
	end
end


--给收集子节点函数用的过滤器
--参数：sets=前新的集合中已经收集的节点，nodes=过滤测试的所有节点，node=测试节点，n=测试节点的顺序ID
--返回为当前的循环要如何结束
--BREAK_NONE：不结束
--BREAK_ONE：结束一个节点（如果当前有多个节点在进行测试，那么BREAK_ONE只是结束掉一个节点的子节点收集过程，后续其它节点的子节点收集过程还会继续）
local allCollectors = {
	all = function(sets, nodes, node, n) 
		sets[#sets + 1] = node
		return BREAK_NONE
	end,
	attr = function(self, sets, nodes, node, n)
		if node.attr[self[1]] == self[2] then
			sets[#sets + 1] = node
		end
		return BREAK_NONE
	end,

	[':first'] = function(sets, nodes, node, n)
		sets[#sets + 1] = node
		return BREAK_ALL
	end,
	[':last'] = function(sets, nodes, node, n)
		sets[#sets + 1] = nodes[#nodes]
		return BREAK_ALL
	end,

	[':first-child'] = function(sets, nodes, node, n)
		local child = node[1]
		if child then
			sets[#sets + 1] = child
		end
		return BREAK_ONE
	end,
	[':second-child'] = function(sets, nodes, node, n)
		local child = node[2]
		if child then
			sets[#sets + 1] = child
		end
		return BREAK_ONE
	end,
	[':last-child'] = function(sets, nodes, node, n)
		local child = node[#node]
		if child then
			sets[#sets + 1] = child
		end
		return BREAK_ONE
	end,
	[':even-child'] = function(sets, nodes, node, n)
		for i = 1, #node do
			if bit.band(i, 1) == 0 then
				sets[#sets + 1] = node[i]
			end
		end
		return BREAK_ONE
	end,
	[':odd-child'] = function(sets, nodes, node, n)
		for i = 1, #node do
			if bit.band(i, 1) == 1 then	
				sets[#sets + 1] = node[i]
			end
		end
		return BREAK_ONE
	end,
}

--根据表达式和对子节点的获取方式返回收集函数以及过滤器函数
local function getCollectionAndFilter(expr, howChildren)
	local collector, coll
	local first = string.byte(expr, 1)

	if howChildren == COLLECT_DIRECTCHILDREN then
		coll = collectDirectChildren
	elseif howChildren == COLLECT_ALLCHILDREN then
		coll = collectAllChildren
	end

	if first == 58 then
		--:
		local name = string.match(expr, '^:[%a-]+')
		if not allCollectors[name] then
			return nil, string.format('%s not exists', name)
		end

		collector = string.sub(expr, 1, #name)
		expr = string.trim(string.sub(expr, #name + 1), true, false)

	elseif first == 91 then
		--[
		local pos

		pos = string.find(expr, ']', 3, true)
		if not pos then
			return nil, string.format('"%s" not closed', expr)
		end

		--取出key&value
		local name, val = string.cut(string.sub(expr, 2, pos - 1), '=')
		name, val = string.trim(name), string.trim(val)

		if not name or not val then
			return nil, string.format('"%s" is not a key=>value expression', expr)
		end

		--去掉值上左右的字符串
		first = string.byte(val, 1)
		if first == 34 or first == 39 then
			local quote = string.byte(val, #val)
			if quote == first then
				val = string.sub(val, 2, #val - 1)
			else
				return nil, string.format('"%s" not paired closed', expr)
			end
		end

		--使用Table模拟函数
		collector = setmetatable({ name, val }, {
			__call = allCollectors.attr
		})
		
		expr = string.trim(string.sub(expr, pos + 1), true, false)
	end

	if type(collector) ~= 'table' then
		collector = allCollectors[collector or 'all']
	end

	return coll, collector, expr
end


--根据表达式匹配所有合适的并返回匹配完成的集合
local function matchAll(self, sets, nodes, expr, howChildren)
	local breakIt = false
	local coll, collector
	local first = string.byte(expr, 1)

	if not sets then
		sets = table.new(4, 0)
	end

	if first == 46 then
		--.
		local className = string.match(string.sub(expr, 2), '^[%w-_]+')
		if not className then
			return 1
		end

		expr = string.sub(expr, 2 + #className)
		coll, collector, expr = getCollectionAndFilter(expr, howChildren)
		if not coll then
			return collector
		end

		for i = 1, #nodes do
			if coll(sets, nodes[i], self.byClasses[className], collector) == BREAK_ALL then
				breakIt = true
				break
			end
		end

	elseif first == 35 then
		--#，按ID查找，结果只会有一个
		local id = string.match(string.sub(expr, 2), '^[%a-_]+')
		if not id then
			return '# syntax error'
		end

		expr = string.sub(expr, 2 + #id)
		coll, collector, expr = getCollectionAndFilter(expr, howChildren)
		if not coll then
			return collector
		end

		local node = self.byIds[id]
		if not node then
			return string.format('"#%s" not exists', id)
		end

		if coll(sets, nil, { node }, collector) == BREAK_ALL then
			breakIt = true
		end

	elseif #expr > 0 then
		--按照标签名称查找
		local tagName = string.match(expr, '^[%a]+')
		if tagName then
			expr = string.sub(expr, 1 + #tagName)
		elseif string.countchars(string.sub(expr, 1, 1), ':[') == 0 then
			return string.format('illegal first-character for expression "%s"', expr)
		end

		coll, collector, expr = getCollectionAndFilter(expr, howChildren)
		if not coll then
			return collector
		end

		if tagName then
			local testNodes = self.byTagNames[tagName]
			if testNodes then
				for i = 1, #nodes do
					if coll(sets, nodes[i], testNodes, collector) == BREAK_ALL then
						breakIt = true
						break
					end
				end
			end
		else
			if coll(sets, nil, nodes, collector) == BREAK_ALL then
				breakIt = true
			end
		end
	end

	if #expr > 0 then
		--继续递归
		local subMatches = table.new(4, 0)
		first = string.byte(expr, 1)
		if first == 32 then
			howChildren = COLLECT_ALLCHILDREN
			expr = string.sub(expr, 2)

		elseif first == 62 then		-->
			howChildren = COLLECT_DIRECTCHILDREN
			expr = string.sub(expr, 2)

		elseif first == 43 then		--+
		elseif first == 126 then	--~
		end

		sets, breakIt = matchAll(self, nil, sets, expr, howChildren)
		if type(sets) ~= 'table' then
			return sets
		end

		return sets, breakIt
	end

	return sets, breakIt
end

local queryMeta = {}
queryMeta.__index = {
	find = function(self, a, howChildren)
		local sets
	
		if a == nil or a == '' then
			sets = table.new(#self, 0)
			for i = 1, #self do
				sets[i] = self[i]
			end

		elseif type(a) == 'string' then
			sets = table.new(8, 0)

			--按逗号拆开多个表达式
			local exprs = string.split(string.trim(a), ',')
			for i = 1, #exprs do
				--逐个表达式的运行
				local expr = exprs[i]
				local r = matchAll(self, nil, self, expr, howChildren or COLLECT_ALLCHILDREN)
				if type(r) == 'number' then
					error(string.format('Selection expression error at [%d]: %s', r, expr))
				elseif type(r) == 'string' then
					error(string.format('Selection expression "%s" error at %s', expr, r))
				elseif type(r) == 'table' then
					for i = 1, #r do
						sets[#sets + 1] = r[i]
					end
				end
			end
		end

		--新的结合使用同一个MetaTable
		return setmetatable(sets, getmetatable(self))
	end,

	children = function(self, a)
		return self:find(a, COLLECT_DIRECTCHILDREN)
	end,
}

queryMeta.__call = function(self, a, b)
	if type(a) == 'string' then
		return self:find(a)

	elseif type(a) == 'table' then
		if #a > 0 and not getmetatable(a) then
			if a[1].tag and a[1].attr then
				return setmetatable(sets, getmetatable(self))
			end
		elseif a.tag and a.attr then
			return 
		end
	end
end

--------------------------------------------------------------------------------
-- gumbo
--------------------------------------------------------------------------------
gumbo.parse = function(input, createQuery)
	if type(input) ~= 'string' or #input < 1 then
		return nil
	end

	local luaQuery
	local output = libgumbo.gumbo_parse(input)

	if createQuery then
		luaQuery = {
			__call = queryMeta.__call,
			__index = setmetatable({
				byClasses = table.new(0, 16),
				byTagNames = table.new(0, 16),
				byNames = table.new(0, 8),
				byIds = table.new(0, 32),
			}, queryMeta)
		}
	end

	local root = transform_gumbo_node_to_lom_node(output.root, luaQuery.__index, 1)
	libgumbo.gumbo_destroy_output(libgumbo.kGumboDefaultOptions, output)
	output = nil

	if luaQuery then
		return setmetatable({ root }, luaQuery)
	end
	
	return root
end

gumbo.parseUrl = function(url, createQuery)
	if not CURL then
		require('curlwrap')
		if not CURL then
			return nil, 'libcurlwrap not loaded'
		end
	end

	local ok, response = CURL.get(url)
	if not ok then
		return nil, response
	end

	return gumbo.parse(response, createQuery)
end

return gumbo