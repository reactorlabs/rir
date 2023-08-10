
//
// Created by Jakob Hain on 8/9/23.
//

#include "serializeUni.h"
#include "utils/Pool.h"

namespace rir {

/// All flags are set. Flags are only unset in children.
SerialFlags SerialFlags::Inherit(EnumSet<SerialFlag>::Any());
/// AST, not guaranteed RIR, hashed, in source, not in feedback
SerialFlags SerialFlags::Ast(SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource);
/// Not an SEXP, not hashed, in source, not in feedback
SerialFlags SerialFlags::DtContext(SerialFlag::InSource);
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::DtBaseline(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Not an AST, guaranteed RIR, not hashed, not in feedback, not in source
SerialFlags SerialFlags::DtOptimized(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp);
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::FunBody(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::FunDefaultArg(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Not an SEXP, hashed, in source, not in feedback
SerialFlags SerialFlags::FunMiscBytes(SerialFlag::Hashed, SerialFlag::InSource);
/// Not an AST, guaranteed rir, hashed, in source, not in feedback
SerialFlags SerialFlags::CodeArglistOrder(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource);
/// Child promise in extra pool
///
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::CodePromise(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Data is part of a record_ bytecode. SEXP is a recorded call in extra pool.
///
/// Not an AST, not guaranteed rir, not hashed, not in source, in feedback
SerialFlags SerialFlags::CodeFeedback(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::InFeedback);
/// Unclassified SEXP in extra pool: original bytecode, any pool entry in
/// native code.
///
/// Not an AST, not guaranteed rir, hashed, not in source, not in feedback
SerialFlags SerialFlags::CodePoolUnknown(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed);
/// Code kind (i.e. whether the code is native) and native code.
///
/// Not an SEXP, hashed, not in source, not in feedback
SerialFlags SerialFlags::CodeNative(SerialFlag::Hashed);
/// Not an SEXP, hashed, in source, not in feedback
SerialFlags SerialFlags::CodeMiscBytes(SerialFlag::Hashed, SerialFlag::InSource);

void Serializer::writeConst(unsigned idx) { write(Pool::get(idx), SerialFlags::Inherit); }

void Serializer::writeSrc(unsigned idx) {
    write(src_pool_at(idx), SerialFlags::Ast);
}

unsigned Deserializer::readConst() { return Pool::insert(read(SerialFlags::Inherit)); }

unsigned Deserializer::readSrc() { return src_pool_add(read(SerialFlags::Ast)); }

} // namespace rir
