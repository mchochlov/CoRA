package com.woodplc.cora.data;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.github.gumtreediff.tree.ITree;
import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;

public final class SubprogramMetadata {

	private final Set<String> arguments = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
	private final Map<ITree, ITree> leavesToSelectedNodes = new HashMap<>();
	private final SetMultimap<ITree, String> nodesToIdentifiers = SetMultimapBuilder.hashKeys().treeSetValues(String.CASE_INSENSITIVE_ORDER).build();

	private SubprogramMetadata() {
		
	}
	
	public static SubprogramMetadata fromITree(ITree root) {

		SubprogramMetadata md = new SubprogramMetadata();
		for (ITree node : root.preOrder()) {
			if (node.getType().name.equals("typeStatementName")) {
				if (node.getChild(0).getType().name.equals("identifier")) {
					md.arguments.add(node.getChild(0).getLabel());
				} else if (node.getChild(0).getType().name.equals("arrayDeclarator")) {
					md.arguments.add(node.getChild(0).getChild(0).getLabel());
				}
			} else if (node.getType().name.equals("assignmentStatement")) {
				//map leafes
				for (ITree child : node.preOrder()) {
					if (child.isLeaf()) {
						md.leavesToSelectedNodes.put(child, node);
					}
				}
				//find identifier(s) in LHS
				ITree child = node.getChild(0);
				while (child != null) {
					child = child.getChild(0);
					if (child.getType().name.equals("identifier")) {
						md.nodesToIdentifiers.put(node, child.getLabel());
						break;
					}
				}
			} else if (node.getType().name.equals("callStatement")) {
				//map leafes
				for (ITree child : node.preOrder()) {
					if (child.isLeaf()) {
						md.leavesToSelectedNodes.put(child, node);
					} else if (child.getType().name.equals("callArgument")) {
						md.nodesToIdentifiers.put(node, child.getChild(0).getChild(0).getLabel());
					}
				}
			} else if (node.getType().name.equals("ifStatement")) {
				for (ITree child : node.preOrder()) {
					if (child.isLeaf()) {
						md.leavesToSelectedNodes.put(child, node);
					} else if (child.getType().name.equals("logicalExpression")) {
						for (ITree c : child.preOrder()) {
							if (c.getType().name.equals("identifier")) {
								md.nodesToIdentifiers.put(node, c.getLabel());
							}
						}
					}
				}
			}
		}
		
		
		return md;
	}

	public Set<String> getArguments() {
		return this.arguments;
	}

	public boolean isSFRefactorable(ITree leaf, final Set<String> commonArguments, Set<String> localArguments) {
		ITree node = leavesToSelectedNodes.get(leaf);
		if (node == null) {
			return false;
		}
		
		Set<String> identifiers = nodesToIdentifiers.get(node);
		if (!identifiers.isEmpty() && 
				!Sets.intersection(commonArguments, identifiers).isEmpty()) {
				return false;
		}
		localArguments.addAll(identifiers);
		return true;
	}
	
	
}
