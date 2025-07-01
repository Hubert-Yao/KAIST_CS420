use std::collections;
use std::fmt::format;
use std::io::{Result, Write};

use itertools::concat;
use lang_c::ast::*;
use lang_c::span::Node;

use crate::write_base::*;

impl<T: WriteLine> WriteLine for Node<T> {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.node.write_line(indent, write)
    }
}

impl<T: WriteString> WriteString for Node<T> {
    fn write_string(&self) -> String {
        self.node.write_string()
    }
}

impl WriteLine for TranslationUnit {
    /// VERY BIG HINT: You should start by understanding the [`writeln!`](https://doc.rust-lang.org/std/macro.writeln.html) macro.
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        for node in &self.0 {
            node.write_line(indent, write)?;
        }
        Ok(())
    }
}

impl WriteLine for ExternalDeclaration {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            ExternalDeclaration::Declaration(dec) => {
                dec.write_line(indent, write)?;
            }
            ExternalDeclaration::StaticAssert(staass) => {
                staass.write_line(indent, write)?;
            }
            ExternalDeclaration::FunctionDefinition(funcdef) => {
                funcdef.write_line(indent, write)?;
            }
        }
        Ok(())
    }
}

impl WriteLine for Declaration {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        // 1. indent
        write!(write, "{:indent$}", "", indent = indent)?;

        // 2. specifiers
        for spec in &self.specifiers {
            write!(write, "{} ", spec.write_string())?;
        }

        //3. variable list
        for (i, init) in self.declarators.iter().enumerate() {
            if i > 0 {
                write!(write, ", ")?;
            }

            write!(write, "{}", init.node.declarator.write_string())?;

            if let Some(expr) = &init.node.initializer {
                write!(write, " = {}", expr.write_string())?;
            }
        }

        // 4. ; and next line
        writeln!(write, ";")?;
        Ok(())
    }
}

impl WriteLine for StaticAssert {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write!(write, "{:indent$}", "", indent = indent)?;

        let expr_str = self.expression.write_string();
        let msg_str = self.message.write_string();
        write!(write, "_Static_assert({}, {});", expr_str, msg_str)?;

        writeln!(write)?;

        Ok(())
    }
}

impl WriteLine for FunctionDefinition {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        // 1. indent
        write!(write, "{:indent$}", "", indent = indent)?;

        // 2. specifiers
        for spec in &self.specifiers {
            write!(write, "{} ", spec.write_string())?;
        }

        // 3. declarator (function name and var_list)
        write!(write, "{} ", self.declarator.write_string())?;

        // 4. Function Context
        self.statement.write_line(indent, write)?;

        Ok(())
    }
}

// TODO
impl WriteLine for Statement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Statement::Labeled(ls) => {
                ls.write_line(indent, write)?;
            }

            Statement::Compound(items) => {
                writeln!(write, "{{")?;

                for bi in items {
                    bi.write_line(indent + 4, write)?;
                }

                writeln!(write, "{:indent$}}}", "", indent = indent)?;
            }

            // ? why ; is always
            Statement::Expression(opt_expr) => {
                write!(write, "{:indent$}", "", indent = indent)?;

                if let Some(e) = opt_expr {
                    write!(write, "{}", e.write_string())?;
                }

                writeln!(write, ";")?;
            }

            Statement::If(if_node) => {
                if_node.write_line(indent, write)?;
            }

            Statement::While(while_node) => {
                while_node.write_line(indent, write)?;
            }

            Statement::For(for_node) => {
                for_node.write_line(indent, write)?;
            }

            Statement::DoWhile(dowhile_node) => {
                dowhile_node.write_line(indent, write)?;
            }

            Statement::Switch(sw_node) => {
                sw_node.write_line(indent, write)?;
            }

            Statement::Return(opt_re_expr) => {
                write!(write, "{:indent$}return", "", indent = indent)?;
                if let Some(expr) = opt_re_expr {
                    write!(write, " {}", expr.write_string())?;
                }
                writeln!(write, ";")?;
            }

            Statement::Continue => {
                writeln!(write, "{:indent$}continue;", "", indent = indent)?;
            }

            Statement::Break => {
                writeln!(write, "{:indent$}break;", "", indent = indent)?;
            }

            other => write!(write, "{:#?}", other)?,
        }

        Ok(())
    }
}

impl WriteLine for LabeledStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        // 1. indent
        write!(write, "{:indent$}", "", indent = indent)?;

        // 2. label
        write!(write, "{} ", self.label.write_string())?;

        // 3. statement
        self.statement.write_line(indent, write)?;

        Ok(())
    }
}

impl WriteLine for BlockItem {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            BlockItem::Declaration(dec) => dec.write_line(indent, write)?,
            BlockItem::StaticAssert(sa) => sa.write_line(indent, write)?,
            BlockItem::Statement(stmt) => stmt.write_line(indent, write)?,
        }

        Ok(())
    }
}

impl WriteLine for IfStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        // if(cond)
        let cond_str = self.condition.write_string();

        write!(write, "{:indent$}if ({}) ", "", cond_str, indent = indent)?;

        // then arm
        self.then_statement.write_line(indent, write)?;

        // else arm (optional)
        if let Some(else_stmt) = &self.else_statement {
            write!(write, "{:indent$}else ", "", indent = indent)?;
            else_stmt.write_line(indent, write);
        }

        Ok(())
    }
}

impl WriteLine for ForStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let init_str = self.initializer.write_string();

        let mut cond_str = String::new();
        if let Some(cond) = &self.condition {
            cond_str = cond.write_string();
        }

        let mut step_str = String::new();
        if let Some(step) = &self.step {
            step_str = step.write_string();
        }

        write!(
            write,
            "{:indent$}for ({}; {}; {}) ",
            "",
            init_str,
            cond_str,
            step_str,
            indent = indent
        )?;

        self.statement.write_line(indent, write);

        Ok(())
    }
}

impl WriteLine for WhileStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let cond = self.expression.write_string();
        write!(write, "{:indent$}while ({}) ", "", cond, indent = indent)?;
        self.statement.write_line(indent, write)?;

        Ok(())
    }
}

impl WriteLine for DoWhileStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write!(write, "{:indent$}do ", "", indent = indent)?;

        self.statement.write_line(indent, write)?;

        let cond = self.expression.write_string();
        writeln!(write, "{:indent$}while ({});", "", cond, indent = indent)?;

        Ok(())
    }
}

impl WriteLine for SwitchStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let expr = self.expression.write_string();
        write!(write, "{:indent$}switch ({}) ", "", expr, indent = indent)?;
        self.statement.write_line(indent, write)?;
        Ok(())
    }
}

// *******************************
// WriteString Implementation for T
// ********************************

impl WriteString for DeclarationSpecifier {
    fn write_string(&self) -> String {
        match self {
            DeclarationSpecifier::StorageClass(scs) => scs.write_string(),
            DeclarationSpecifier::TypeSpecifier(ts) => ts.write_string(),
            DeclarationSpecifier::TypeQualifier(tq) => tq.write_string(),
            DeclarationSpecifier::Function(fs) => fs.write_string(),
            DeclarationSpecifier::Alignment(als) => als.write_string(),
            DeclarationSpecifier::Extension(exts) => exts
                .iter()
                .map(|e| e.write_string())
                .collect::<Vec<_>>()
                .join(" "),
        }
    }
}

impl WriteString for StorageClassSpecifier {
    fn write_string(&self) -> String {
        match self {
            StorageClassSpecifier::Typedef => "typedef".into(),
            StorageClassSpecifier::Extern => "extern".into(),
            StorageClassSpecifier::Static => "static".into(),
            StorageClassSpecifier::ThreadLocal => "_Thread_local".into(),
            StorageClassSpecifier::Auto => "auto".into(),
            StorageClassSpecifier::Register => "register".into(),
        }
    }
}

impl WriteString for FunctionSpecifier {
    fn write_string(&self) -> String {
        match self {
            FunctionSpecifier::Inline => "inline".into(),
            FunctionSpecifier::Noreturn => "_Noreturn".into(),
        }
    }
}

impl WriteString for AlignmentSpecifier {
    fn write_string(&self) -> String {
        match self {
            AlignmentSpecifier::Type(ty) => format!("_Alignment({})", ty.write_string()),
            AlignmentSpecifier::Constant(expr) => format!("_Alignment({})", expr.write_string()),
        }
    }
}

impl WriteString for Declarator {
    fn write_string(&self) -> String {
        let mut prefix = String::new();
        let mut suffix = String::new();

        for drv in &self.derived {
            match &drv.node {
                DerivedDeclarator::Pointer(_) => {
                    prefix.push_str(&drv.write_string());
                }
                _ => {
                    suffix.push_str(&drv.write_string());
                }
            }
        }

        let mut s = String::new();

        s.push_str(&prefix);
        s.push_str(&self.kind.write_string());
        s.push_str(&suffix);

        if !self.extensions.is_empty() {
            let exts = self
                .extensions
                .iter()
                .map(|e| e.write_string())
                .collect::<Vec<_>>()
                .join(" ");
            s.push(' ');
            s.push_str(&exts);
        }

        s
    }
}

impl WriteString for DeclaratorKind {
    fn write_string(&self) -> String {
        match self {
            DeclaratorKind::Abstract => String::new(),
            DeclaratorKind::Identifier(id_node) => id_node.write_string(),
            DeclaratorKind::Declarator(inner_node) => format!("({})", inner_node.write_string()),
        }
    }
}

impl WriteString for DerivedDeclarator {
    fn write_string(&self) -> String {
        match self {
            DerivedDeclarator::Pointer(pq_node_s) => {
                let qs = pq_node_s
                    .iter()
                    .map(|q| q.write_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                if qs.is_empty() {
                    "*".into()
                } else {
                    format!("* {}", qs)
                }
            }

            DerivedDeclarator::Array(arr_node) => arr_node.write_string(),

            DerivedDeclarator::Function(f_node) => f_node.write_string(),

            DerivedDeclarator::KRFunction(ids) => {
                let names = ids
                    .iter()
                    .map(|id| id.write_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", names)
            }

            DerivedDeclarator::Block(pqs) => {
                let qs = pqs
                    .iter()
                    .map(|q| q.write_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                if qs.is_empty() {
                    "^".into()
                } else {
                    format!("^ {}", qs)
                }
            }
        }
    }
}

impl WriteString for PointerQualifier {
    fn write_string(&self) -> String {
        match self {
            PointerQualifier::TypeQualifier(tq_node) => tq_node.write_string(),
            PointerQualifier::Extension(exts) => exts
                .iter()
                .map(|e| e.write_string())
                .collect::<Vec<_>>()
                .join(" "),
        }
    }
}

impl WriteString for ArrayDeclarator {
    fn write_string(&self) -> String {
        let quals = self
            .qualifiers
            .iter()
            .map(|q| q.write_string())
            .collect::<Vec<_>>()
            .join(" ");

        let size_str = self.size.write_string();

        let inner = if !quals.is_empty() && !size_str.is_empty() {
            format!("{} {}", quals, size_str)
        } else if !quals.is_empty() {
            quals
        } else {
            size_str
        };

        format!("[{}]", inner)
    }
}

impl WriteString for ArraySize {
    fn write_string(&self) -> String {
        match self {
            ArraySize::Unknown => String::new(),
            ArraySize::VariableUnknown => "*".into(),
            ArraySize::VariableExpression(expr) => expr.write_string(),
            ArraySize::StaticExpression(expr) => format!("static {}", expr.write_string()),
        }
    }
}

impl WriteString for FunctionDeclarator {
    fn write_string(&self) -> String {
        let mut params = self
            .parameters
            .iter()
            .map(|p| p.write_string())
            .collect::<Vec<_>>();

        if let Ellipsis::Some = self.ellipsis {
            params.push(self.ellipsis.write_string());
        }

        format!("({})", params.join(", "))
    }
}

impl WriteString for ParameterDeclaration {
    fn write_string(&self) -> String {
        let mut s = self
            .specifiers
            .iter()
            .map(|sp| sp.write_string())
            .collect::<Vec<_>>()
            .join(" ");

        if let Some(decl) = &self.declarator {
            if !s.is_empty() {
                s.push(' ');
            }
            s.push_str(&decl.write_string());
        }

        if !self.extensions.is_empty() {
            if !s.is_empty() {
                s.push(' ');
            }

            let exts = self
                .extensions
                .iter()
                .map(|e| e.write_string())
                .collect::<Vec<_>>()
                .join(" ");
            s.push_str(&exts);
        }

        s
    }
}

impl WriteString for Ellipsis {
    fn write_string(&self) -> String {
        match self {
            Ellipsis::Some => "...".to_string(),
            Ellipsis::None => String::new(),
        }
    }
}

impl WriteString for Initializer {
    fn write_string(&self) -> String {
        match self {
            Initializer::Expression(expr_node) => expr_node.write_string(),
            Initializer::List(items) => {
                let inner = items
                    .iter()
                    .map(|item| item.write_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{{ {} }}", inner)
            }
        }
    }
}

impl WriteString for Expression {
    fn write_string(&self) -> String {
        match self {
            Expression::Identifier(node) => node.write_string(),
            Expression::Constant(node) => node.write_string(),
            Expression::StringLiteral(node) => node.write_string(),
            Expression::GenericSelection(node) => node.write_string(),
            Expression::Member(node) => node.write_string(),
            Expression::Call(node) => node.write_string(),
            Expression::CompoundLiteral(node) => node.write_string(),
            Expression::SizeOfTy(node) => node.write_string(),
            Expression::SizeOfVal(node) => node.write_string(),
            Expression::AlignOf(node) => node.write_string(),
            Expression::UnaryOperator(node) => node.write_string(),
            Expression::Cast(node) => node.write_string(),
            Expression::BinaryOperator(node) => node.write_string(),
            Expression::Conditional(node) => node.write_string(),
            Expression::Comma(expr_node_s) => {
                let expr = expr_node_s
                    .iter()
                    .map(|e| e.write_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", expr)
            }
            Expression::OffsetOf(node) => node.write_string(),
            Expression::VaArg(node) => node.write_string(),

            // TODO Just for statement (GNU extension)
            other => format!("{:?}", other),
        }
    }
}

impl WriteString for Identifier {
    fn write_string(&self) -> String {
        self.name.clone()
    }
}

impl WriteString for Constant {
    fn write_string(&self) -> String {
        match self {
            Constant::Integer(itg) => itg.write_string(),
            Constant::Float(flt) => flt.write_string(),
            Constant::Character(ch) => ch.clone(),
        }
    }
}

impl WriteString for Float {
    fn write_string(&self) -> String {
        let prefix = self.base.write_string();
        let num = self.number.to_string();
        let suffix = self.suffix.write_string();

        format!("{}{}{}", prefix, num, suffix)
    }
}

impl WriteString for FloatBase {
    fn write_string(&self) -> String {
        match self {
            FloatBase::Hexadecimal => "0x".into(),
            FloatBase::Decimal => String::new(),
        }
    }
}

impl WriteString for FloatSuffix {
    fn write_string(&self) -> String {
        let fmt_suffix = self.format.write_string();
        let imag = if self.imaginary { "i" } else { "" };

        format!("{}{}", fmt_suffix, imag)
    }
}

impl WriteString for FloatFormat {
    fn write_string(&self) -> String {
        match self {
            FloatFormat::Float => "f".into(),
            FloatFormat::Double => String::new(),
            FloatFormat::LongDouble => "l".into(),
            // NOT IMPLEMENT
            FloatFormat::TS18661Format(_) => String::new(),
        }
    }
}

impl WriteString for Integer {
    fn write_string(&self) -> String {
        let mut base = self.base.write_string();

        let number = self.number.to_string();

        let suffix = self.suffix.write_string();

        format!("{}{}{}", base, number, suffix)
    }
}

impl WriteString for IntegerBase {
    fn write_string(&self) -> String {
        match self {
            IntegerBase::Decimal => String::new(),
            IntegerBase::Octal => "0".into(),
            IntegerBase::Hexadecimal => "0x".into(),
            IntegerBase::Binary => "0b".into(),
        }
    }
}

impl WriteString for IntegerSuffix {
    fn write_string(&self) -> String {
        let mut res = String::new();

        let size = self.size.write_string();
        res.push_str(&size);

        if self.unsigned {
            res.push('u');
        }

        if self.imaginary {
            res.push('i');
        }

        res
    }
}

impl WriteString for IntegerSize {
    fn write_string(&self) -> String {
        match self {
            IntegerSize::Int => String::new(),
            IntegerSize::Long => "l".into(),
            IntegerSize::LongLong => "ll".into(),
        }
    }
}

impl WriteString for StringLiteral {
    fn write_string(&self) -> String {
        self.join("")
    }
}

impl WriteString for GenericSelection {
    fn write_string(&self) -> String {
        // 1. 控制表达式
        let expr_str = self.expression.write_string();

        // 各个association
        let assoc_list = self
            .associations
            .iter()
            .map(|assoc| assoc.write_string())
            .collect::<Vec<_>>()
            .join(", ");

        format!("_Generic({}, {})", expr_str, assoc_list)
    }
}

impl WriteString for GenericAssociation {
    fn write_string(&self) -> String {
        match self {
            GenericAssociation::Type(gat_node) => gat_node.write_string(),
            GenericAssociation::Default(expr_node) => expr_node.write_string(),
        }
    }
}

impl WriteString for GenericAssociationType {
    fn write_string(&self) -> String {
        format!(
            "{}: {}",
            self.type_name.write_string(),
            self.expression.write_string()
        )
    }
}

impl WriteString for TypeName {
    fn write_string(&self) -> String {
        // 1. connect all specifier / qualifier WITH " "
        let specs = self
            .specifiers
            .iter()
            .map(|sq| sq.write_string())
            .collect::<Vec<_>>()
            .join(" ");

        // 2. declarator
        if let Some(decl) = &self.declarator {
            format!("{} {}", specs, decl.write_string())
        } else {
            specs
        }
    }
}

impl WriteString for SpecifierQualifier {
    fn write_string(&self) -> String {
        match self {
            SpecifierQualifier::TypeSpecifier(ts_node) => ts_node.write_string(),
            SpecifierQualifier::TypeQualifier(tq_node) => tq_node.write_string(),
            SpecifierQualifier::Extension(exts) => exts
                .iter()
                .map(|e| e.write_string())
                .collect::<Vec<_>>()
                .join(" "),
        }
    }
}

impl WriteString for TypeSpecifier {
    fn write_string(&self) -> String {
        match self {
            // 1. basic
            TypeSpecifier::Void => "void".into(),
            TypeSpecifier::Char => "char".into(),
            TypeSpecifier::Short => "short".into(),
            TypeSpecifier::Int => "int".into(),
            TypeSpecifier::Long => "long".into(),
            TypeSpecifier::Float => "float".into(),
            TypeSpecifier::Double => "double".into(),
            TypeSpecifier::Signed => "signed".into(),
            TypeSpecifier::Unsigned => "unsigned".into(),
            TypeSpecifier::Bool => "_Bool".into(),
            TypeSpecifier::Complex => "_Complex".into(),

            // 2. atomic
            TypeSpecifier::Atomic(at_node) => {
                format!("_Atomic({})", at_node.write_string())
            }

            // 3. Struct
            TypeSpecifier::Struct(st_node) => st_node.write_string(),

            // 4. Enum
            TypeSpecifier::Enum(en_node) => en_node.write_string(),

            // 5. Typedef
            TypeSpecifier::TypedefName(id_node) => id_node.write_string(),

            //  JUST DEBUG OUTPUT (TODO)
            other => format!("{:?}", other),
        }
    }
}

impl WriteString for StructType {
    fn write_string(&self) -> String {
        let kw = self.kind.write_string();
        let head = if let Some(id) = &self.identifier {
            format!("{} {}", kw, id.write_string())
        } else {
            kw
        };

        let decls = match &self.declarations {
            Some(d) => d,
            None => return head,
        };

        let mut members = decls
            .iter()
            .map(|decl| decl.write_string())
            .collect::<Vec<_>>()
            .join("; ");

        if !decls.is_empty() {
            members.push(';');
        }

        format!("{} {{ {} }}", head, members)
    }
}

impl WriteString for StructKind {
    fn write_string(&self) -> String {
        match self {
            StructKind::Struct => "struct".to_string(),
            StructKind::Union => "union".to_string(),
        }
    }
}

impl WriteString for StructDeclaration {
    fn write_string(&self) -> String {
        match self {
            StructDeclaration::Field(field_node) => field_node.write_string(),
            StructDeclaration::StaticAssert(sa_node) => sa_node.write_string(),
        }
    }
}

impl WriteString for StructField {
    fn write_string(&self) -> String {
        let specs = self
            .specifiers
            .iter()
            .map(|sq| sq.write_string())
            .collect::<Vec<_>>()
            .join(" ");

        let decls = self
            .declarators
            .iter()
            .map(|sd| sd.write_string())
            .collect::<Vec<_>>()
            .join(", ");

        match (specs.is_empty(), decls.is_empty()) {
            (false, false) => format!("{} {}", specs, decls),
            (false, true) => specs,
            (true, false) => decls,
            (true, true) => String::new(),
        }
    }
}

impl WriteString for StructDeclarator {
    fn write_string(&self) -> String {
        let mut s = String::new();

        if let Some(d) = &self.declarator {
            s.push_str(&d.write_string());
        }

        if let Some(bw) = &self.bit_width {
            let w = bw.write_string();
            s.push_str(" : ");
            s.push_str(&w);
        }

        s
    }
}

impl WriteString for EnumType {
    fn write_string(&self) -> String {
        if let Some(id) = &self.identifier {
            format!("enum {}", id.write_string())
        } else {
            "enum".into()
        }
    }
}

impl WriteString for TypeQualifier {
    fn write_string(&self) -> String {
        match self {
            // 1. C11 标准限定符
            TypeQualifier::Const => "const".into(),
            TypeQualifier::Restrict => "restrict".into(),
            TypeQualifier::Volatile => "volatile".into(),

            // 2. Clang 可空性扩展
            TypeQualifier::Nonnull => "_Nonnull".into(),
            TypeQualifier::NullUnspecified => "_Null_unspecified".into(),
            TypeQualifier::Nullable => "_Nullable".into(),

            // 3. C11 原子限定符
            TypeQualifier::Atomic => "_Atomic".into(),
        }
    }
}

// TODO
impl WriteString for Extension {
    fn write_string(&self) -> String {
        format!("{:?}", self)
    }
}

impl WriteString for MemberExpression {
    fn write_string(&self) -> String {
        let op = self.operator.write_string();

        let lhs = self.expression.write_string();

        let rhs = self.identifier.write_string();

        format!("{}{}{}", lhs, op, rhs)
    }
}

impl WriteString for MemberOperator {
    fn write_string(&self) -> String {
        match self {
            MemberOperator::Direct => ".".to_string(),
            MemberOperator::Indirect => "->".to_string(),
        }
    }
}

impl WriteString for CallExpression {
    fn write_string(&self) -> String {
        let callee_str = self.callee.write_string();

        let args_str = self
            .arguments
            .iter()
            .map(|e| e.write_string())
            .collect::<Vec<_>>()
            .join(", ");

        format!("{}({})", callee_str, args_str)
    }
}

impl WriteString for CompoundLiteral {
    fn write_string(&self) -> String {
        let ty = self.type_name.write_string();

        let items = self
            .initializer_list
            .iter()
            .map(|item| item.write_string())
            .collect::<Vec<_>>()
            .join(", ");

        format!("({}) {{ {} }}", ty, items)
    }
}

impl WriteString for InitializerListItem {
    fn write_string(&self) -> String {
        let desig = self
            .designation
            .iter()
            .map(|d| d.write_string())
            .collect::<Vec<_>>()
            .join("");

        let val = self.initializer.write_string();

        if desig.is_empty() {
            val
        } else {
            format!("{} = {}", desig, val)
        }
    }
}

impl WriteString for Designator {
    fn write_string(&self) -> String {
        match self {
            Designator::Index(idx_node) => format!("[{}]", idx_node.write_string()),
            Designator::Member(id_node) => format!(".{}", id_node.write_string()),
            Designator::Range(rd_node) => format!(
                "[{} ... {}]",
                rd_node.node.from.write_string(),
                rd_node.node.to.write_string()
            ),
        }
    }
}

impl WriteString for SizeOfTy {
    fn write_string(&self) -> String {
        format!("sizeof({})", self.0.write_string())
    }
}

impl WriteString for SizeOfVal {
    fn write_string(&self) -> String {
        format!("sizeof {}", self.0.write_string())
    }
}

impl WriteString for AlignOf {
    fn write_string(&self) -> String {
        format!("_Alignof({})", self.0.write_string())
    }
}

impl WriteString for UnaryOperatorExpression {
    fn write_string(&self) -> String {
        let op = self.operator.write_string();

        let expr = self.operand.write_string();

        match self.operator.node {
            UnaryOperator::PostIncrement | UnaryOperator::PostDecrement => {
                format!("{}{}", expr, op)
            }

            _ => format!("{}({})", op, expr),
        }
    }
}

impl WriteString for UnaryOperator {
    fn write_string(&self) -> String {
        match self {
            // postfix
            UnaryOperator::PostIncrement => "++".into(),
            UnaryOperator::PostDecrement => "--".into(),
            // prefix
            UnaryOperator::PreIncrement => "++".into(),
            UnaryOperator::PreDecrement => "--".into(),
            // 取地址／解引用
            UnaryOperator::Address => "&".into(),
            UnaryOperator::Indirection => "*".into(),
            // 正负号
            UnaryOperator::Plus => "+".into(),
            UnaryOperator::Minus => "-".into(),
            // 位翻转／逻辑非
            UnaryOperator::Complement => "~".into(),
            UnaryOperator::Negate => "!".into(),
        }
    }
}

impl WriteString for CastExpression {
    fn write_string(&self) -> String {
        let ty_str = self.type_name.write_string();

        let expr_str = self.expression.write_string();

        format!("({}) {}", ty_str, expr_str)
    }
}

impl WriteString for BinaryOperatorExpression {
    fn write_string(&self) -> String {
        let lhs = self.lhs.write_string();
        let rhs = self.rhs.write_string();
        let op = self.operator.write_string();
        match self.operator.node {
            BinaryOperator::Index => format!("({}[{}])", lhs, rhs),
            _ => format!("({} {} {})", lhs, op, rhs),
        }
    }
}

impl WriteString for BinaryOperator {
    fn write_string(&self) -> String {
        match self {
            BinaryOperator::Multiply => "*".into(),
            BinaryOperator::Divide => "/".into(),
            BinaryOperator::Modulo => "%".into(),
            BinaryOperator::Plus => "+".into(),
            BinaryOperator::Minus => "-".into(),
            BinaryOperator::ShiftLeft => "<<".into(),
            BinaryOperator::ShiftRight => ">>".into(),
            BinaryOperator::Less => "<".into(),
            BinaryOperator::Greater => ">".into(),
            BinaryOperator::LessOrEqual => "<=".into(),
            BinaryOperator::GreaterOrEqual => ">=".into(),
            BinaryOperator::Equals => "==".into(),
            BinaryOperator::NotEquals => "!=".into(),
            BinaryOperator::BitwiseAnd => "&".into(),
            BinaryOperator::BitwiseXor => "^".into(),
            BinaryOperator::BitwiseOr => "|".into(),
            BinaryOperator::LogicalAnd => "&&".into(),
            BinaryOperator::LogicalOr => "||".into(),
            BinaryOperator::Assign => "=".into(),
            BinaryOperator::AssignMultiply => "*=".into(),
            BinaryOperator::AssignDivide => "/=".into(),
            BinaryOperator::AssignModulo => "%=".into(),
            BinaryOperator::AssignPlus => "+=".into(),
            BinaryOperator::AssignMinus => "-=".into(),
            BinaryOperator::AssignShiftLeft => "<<=".into(),
            BinaryOperator::AssignShiftRight => ">>=".into(),
            BinaryOperator::AssignBitwiseAnd => "&=".into(),
            BinaryOperator::AssignBitwiseXor => "^=".into(),
            BinaryOperator::AssignBitwiseOr => "|=".into(),
            // 下标访问由 ExpressionImpl 单独处理
            BinaryOperator::Index => "[]".into(),
        }
    }
}

impl WriteString for ConditionalExpression {
    fn write_string(&self) -> String {
        let cond_str = self.condition.write_string();
        let then_str = self.then_expression.write_string();
        let else_str = self.else_expression.write_string();

        format!("(({}) ? ({}) : ({}))", cond_str, then_str, else_str)
    }
}

impl WriteString for OffsetOfExpression {
    fn write_string(&self) -> String {
        let ty = self.type_name.write_string();
        let desig = self.designator.write_string();

        format!("offsetof({}, {})", ty, desig)
    }
}

impl WriteString for OffsetDesignator {
    fn write_string(&self) -> String {
        let mut s = self.base.write_string();

        for m in &self.members {
            s.push_str(&m.write_string());
        }

        s
    }
}

impl WriteString for OffsetMember {
    fn write_string(&self) -> String {
        match self {
            OffsetMember::Member(id) => format!(".{}", id.write_string()),
            OffsetMember::IndirectMember(id) => format!("->{}", id.write_string()),
            OffsetMember::Index(expr) => format!("[{}]", expr.write_string()),
        }
    }
}

impl WriteString for VaArgExpression {
    fn write_string(&self) -> String {
        let list_str = self.va_list.write_string();
        let type_str = self.type_name.write_string();

        format!("va_arg({}, {})", list_str, type_str)
    }
}

impl WriteString for Label {
    fn write_string(&self) -> String {
        match self {
            Label::Identifier(id) => format!("{}:", id.write_string()),
            Label::Case(expr) => format!("case {}:", expr.write_string()),
            Label::CaseRange(r) => format!(
                "case {} ... {}:",
                r.node.low.write_string(),
                r.node.high.write_string()
            ),
            Label::Default => "default:".into(),
        }
    }
}

impl WriteString for ForInitializer {
    fn write_string(&self) -> String {
        match self {
            ForInitializer::Empty => String::new(),
            ForInitializer::Expression(expr) => expr.write_string(),
            ForInitializer::Declaration(dec) => dec.write_string(),
            ForInitializer::StaticAssert(sa) => sa.write_string(),
        }
    }
}

impl WriteString for Declaration {
    fn write_string(&self) -> String {
        let specs = self
            .specifiers
            .iter()
            .map(|sp| sp.write_string())
            .collect::<Vec<_>>()
            .join(" ");

        let vars = self
            .declarators
            .iter()
            .map(|initd| initd.write_string())
            .collect::<Vec<_>>()
            .join(", ");

        if specs.is_empty() {
            vars
        } else if vars.is_empty() {
            specs
        } else {
            format!("{} {}", specs, vars)
        }
    }
}

impl WriteString for InitDeclarator {
    fn write_string(&self) -> String {
        let mut s = self.declarator.write_string();

        if let Some(init) = &self.initializer {
            s.push_str(" = ");
            s.push_str(&init.write_string());
        }

        s
    }
}

impl WriteString for StaticAssert {
    fn write_string(&self) -> String {
        let expr = self.expression.write_string();
        let msg = self.message.write_string();
        format!("_Static_assert({}, {})", expr, msg)
    }
}
