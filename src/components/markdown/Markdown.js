import React from 'react';
import ReactMarkdown from 'react-markdown/with-html';
import './Markdown.css';

const Markdown = (props) => {
  return (
    <div id={props.title}>
      <ReactMarkdown
        source={props.text}
        key={props.title}
        escapeHtml={false}
        className="markdown"
      />
    </div>
  );
};

export default Markdown;
