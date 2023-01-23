use parser::token::SyntaxKind::*;

use crate::ast_node;

ast_node!(SourceFile, SOURCE_FILE);

ast_node!(Item {
    Module(ItemModule, ITEM_MODULE),
});

ast_node!(Name, IDENT);
ast_node!(NameRef, IDENT);
